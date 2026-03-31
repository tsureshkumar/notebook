-- ~/.config/nvim/lua/agent_bridge/server.lua

local M = {}

local uv = vim.uv or vim.loop

M.config = {
  host = "127.0.0.1",
  port = 7777,

  -- If true, allow run_shell after approval prompt.
  -- If false, run_shell always denied (still prompts if you want, but safer).
  enable_shell = true,

  -- Optional: restrict shell cwd to these prefixes (set to nil to allow any).
  allowed_cwd_prefixes = {
    vim.fn.getcwd(), -- default: current working dir
    -- "/Users/you/projects",
  },

  -- Allowlisted ex command prefixes (very coarse; shell-ish still requires approval).
  allowed_ex_prefixes = {
    "set", "silent", "noautocmd", "normal", "keepjumps", "keeppatterns",
    "write", "w", "update", "edit", "e", "b", "buffer",
    "tabnext", "tabprev", "tabnew", "vsplit", "split",
    "lua", "messages", "echo", "redir", "lopen", "copen", "lclose",
    "cclose", "checkhealth"
  },

  -- Where to write last response (optional)
  store_last_response = true,
}

local server ---@type uv_tcp_t|nil
local clients = {} ---@type table<uv_tcp_t, boolean>

local function json_encode(x) return vim.json.encode(x) end
local function json_decode(s) return vim.json.decode(s, { luanil = { object = true, array = true } }) end

local function starts_with(s, prefix) return s:sub(1, #prefix) == prefix end

local function trim_colon(cmd)
  cmd = cmd or ""
  cmd = cmd:gsub("^%s+", ""):gsub("%s+$", "")
  cmd = cmd:gsub("^:%s*", "")
  return cmd
end

-- Detect "shell-capable" commands that must require approval.
local function is_shellish_ex(cmd)
  local c = trim_colon(cmd)

  -- direct shell escape
  if c:match("^!") then return true end

  -- terminal / read/write with bang / grep/make are often external
  if c:match("^terminal%s") then return true end
  if c:match("^read%s*!") then return true end
  if c:match("^write%s*!") then return true end
  if c:match("^make%s") then return true end
  if c:match("^grep%s") then return true end

  -- Lua escape hatches
  if c:match("^lua%s") then
    local s = c:lower()
    if s:find("vim%.fn%.system", 1, true) then return true end
    if s:find("systemlist", 1, true) then return true end
    if s:find("jobstart", 1, true) then return true end
    if s:find("vim%.loop%.spawn", 1, true) then return true end
    if s:find("uv%.spawn", 1, true) then return true end
    if s:find("io%.popen", 1, true) then return true end
    if s:find("os%.execute", 1, true) then return true end
  end

  return false
end

local function is_allowed_ex(cmd)
  local c = trim_colon(cmd)
  local head = c:match("^(%S+)")
  if not head then return false end

  for _, p in ipairs(M.config.allowed_ex_prefixes or {}) do
    if head == p then return true end
  end
  return false
end

local function confirm(prompt, details)
  local msg = prompt
  if details and details ~= "" then
    msg = msg .. "\n\n" .. details
  end
  local choice = vim.fn.confirm(msg, "&Allow\n&Deny", 2)
  return choice == 1
end

local function cwd_allowed(cwd)
  if not cwd or cwd == "" then return true end
  local allowed = M.config.allowed_cwd_prefixes
  if not allowed then return true end
  for _, pref in ipairs(allowed) do
    if pref and pref ~= "" and starts_with(cwd, pref) then
      return true
    end
  end
  return false
end

-- Get diagnostics for current buffer (LSP + others via vim.diagnostic)
local function get_diagnostics(bufnr)
  local diags = vim.diagnostic.get(bufnr)
  local out = {}
  for _, d in ipairs(diags) do
    table.insert(out, {
      lnum = d.lnum, col = d.col,
      end_lnum = d.end_lnum, end_col = d.end_col,
      severity = d.severity,
      message = d.message,
      source = d.source,
      code = d.code,
    })
  end
  return out
end

local function get_visual_selection_range()
  -- Works only if there is an active visual selection; otherwise returns nil.
  local mode = vim.fn.mode()
  if mode ~= "v" and mode ~= "V" and mode ~= "\22" then -- \22 is CTRL-V block
    return nil
  end

  local srow, scol = unpack(vim.api.nvim_buf_get_mark(0, "<"))
  local erow, ecol = unpack(vim.api.nvim_buf_get_mark(0, ">"))
  -- marks are 1-based rows; convert to 0-based
  srow = srow - 1; erow = erow - 1
  return { srow, scol, erow, ecol }
end

local function get_state(req)
  local bufnr = req.bufnr or 0
  local buf = vim.api.nvim_get_current_buf()
  if bufnr ~= 0 then buf = bufnr end

  local name = vim.api.nvim_buf_get_name(buf)
  local cursor = vim.api.nvim_win_get_cursor(0) -- {row(1-based), col(0-based)}
  local selection = get_visual_selection_range()

  -- text range
  local text = nil
  if req.range and type(req.range) == "table" then
    local sr = req.range[1] or 0
    local er = req.range[2] or (vim.api.nvim_buf_line_count(buf))
    text = vim.api.nvim_buf_get_lines(buf, sr, er, true)
  elseif req.include_text then
    text = vim.api.nvim_buf_get_lines(buf, 0, -1, true)
  end

  return {
    ok = true,
    bufnr = buf,
    file = name,
    cursor = { cursor[1] - 1, cursor[2] }, -- normalize to 0-based row
    selection = selection,
    line_count = vim.api.nvim_buf_line_count(buf),
    diagnostics = get_diagnostics(buf),
    text = text,
  }
end

local function sort_edits_desc(edits)
  table.sort(edits, function(a, b)
    local asr, asc = a.start[1], a.start[2]
    local bsr, bsc = b.start[1], b.start[2]
    if asr ~= bsr then return asr > bsr end
    return asc > bsc
  end)
end

local function apply_edits(req)
  local buf = req.bufnr or 0
  if buf == 0 then buf = vim.api.nvim_get_current_buf() end
  local edits = req.edits
  if type(edits) ~= "table" then
    return { ok = false, error = "edits must be an array" }
  end

  -- Validate edit shapes
  for _, e in ipairs(edits) do
    if type(e) ~= "table"
      or type(e.start) ~= "table" or type(e["end"]) ~= "table"
      or type(e.lines) ~= "table"
    then
      return { ok = false, error = "invalid edit format" }
    end
  end

  -- Apply from bottom-to-top so ranges don't shift
  sort_edits_desc(edits)

  local applied = 0
  vim.api.nvim_buf_call(buf, function()
    -- Make it a single undo step
    vim.cmd("silent! undojoin") -- harmless if nothing to join yet
    for i, e in ipairs(edits) do
      if i > 1 then vim.cmd("silent! undojoin") end
      vim.api.nvim_buf_set_text(
        buf,
        e.start[1], e.start[2],
        e["end"][1], e["end"][2],
        e.lines
      )
      applied = applied + 1
    end
  end)

  return { ok = true, applied = applied }
end

local function exec_ex(req)
  local cmd = req.cmd
  if type(cmd) ~= "string" or cmd == "" then
    return { ok = false, error = "cmd must be a non-empty string" }
  end

  -- require allowlist
  if not is_allowed_ex(cmd) then
    return { ok = false, denied = true, reason = "ex command not allowlisted" }
  end

  -- approval gate for shell-ish
  if is_shellish_ex(cmd) then
    local allowed = confirm("Agent requests a shell-capable ex command. Allow?", cmd)
    if not allowed then
      return { ok = false, denied = true, reason = "user denied" }
    end
  end

  local c = trim_colon(cmd)
  local ok, res = pcall(vim.api.nvim_exec2, c, { output = true })
  if not ok then
    return { ok = false, error = tostring(res) }
  end
  return { ok = true, output = res.output or "" }
end

local function run_shell(req)
  local argv = req.argv
  local cwd = req.cwd or vim.fn.getcwd()

  if not M.config.enable_shell then
    return { ok = false, denied = true, reason = "shell disabled in config" }
  end

  if type(argv) ~= "table" or #argv < 1 then
    return { ok = false, error = "argv must be an array like ['git','status']" }
  end

  if not cwd_allowed(cwd) then
    return { ok = false, denied = true, reason = "cwd not allowed", cwd = cwd }
  end

  local pretty = table.concat(argv, " ")
  local allowed = confirm("Agent requests a shell command. Allow?", pretty .. "\n\ncwd: " .. cwd)
  if not allowed then
    return { ok = false, denied = true, reason = "user denied" }
  end

  local timeout_ms = tonumber(req.timeout_ms) or 15000

  -- Neovim 0.10+: vim.system
  local result = vim.system(argv, { cwd = cwd, text = true, timeout = timeout_ms }):wait()
  return {
    ok = true,
    approved = true,
    code = result.code,
    signal = result.signal,
    stdout = result.stdout,
    stderr = result.stderr,
  }
end

local function handle_request(req)
  local op = req.op
  if op == "ping" then
    return { ok = true, pong = true, now = vim.fn.strftime("%Y-%m-%d %H:%M:%S") }
  elseif op == "get_state" then
    return get_state(req)
  elseif op == "apply_edits" then
    return apply_edits(req)
  elseif op == "exec_ex" then
    return exec_ex(req)
  elseif op == "run_shell" then
    return run_shell(req)
  else
    return { ok = false, error = "unknown op: " .. tostring(op) }
  end
end

local function write_response(client, resp)
  local payload = json_encode(resp) .. "\n"
  client:write(payload)
  if M.config.store_last_response then
    vim.g.agent_bridge_last = payload
  end
end

local function on_client_data(client, data_buf, chunk)
  if not chunk then
    clients[client] = nil
    pcall(client.close, client)
    return
  end

  data_buf[1] = data_buf[1] .. chunk

  while true do
    local s = data_buf[1]
    local nl = s:find("\n", 1, true)
    if not nl then break end

    local line = s:sub(1, nl - 1)
    data_buf[1] = s:sub(nl + 1)

    if line ~= "" then
      local ok, req = pcall(json_decode, line)
      if not ok or type(req) ~= "table" then
        write_response(client, { ok = false, error = "bad json" })
      else
        -- Ensure we run Neovim API work on the main thread
        vim.schedule(function()
          local resp = handle_request(req)
          if req.id ~= nil then resp.id = req.id end
          write_response(client, resp)
        end)
      end
    end
  end
end

function M.start(opts)
  M.config = vim.tbl_deep_extend("force", M.config, opts or {})

  if server then
    return { ok = false, error = "already running" }
  end

  server = uv.new_tcp()
  server:bind(M.config.host, M.config.port)

  server:listen(128, function(err)
    if err then
      vim.schedule(function()
        vim.notify("AgentBridge listen error: " .. tostring(err), vim.log.levels.ERROR)
      end)
      return
    end

    local client = uv.new_tcp()
    server:accept(client)
    clients[client] = true

    local data_buf = { "" }
    client:read_start(function(read_err, chunk)
      if read_err then
        clients[client] = nil
        pcall(client.close, client)
        return
      end
      on_client_data(client, data_buf, chunk)
    end)
  end)

  vim.notify(("AgentBridge listening on %s:%d"):format(M.config.host, M.config.port), vim.log.levels.INFO)
  return { ok = true }
end

function M.stop()
  if not server then return { ok = true } end
  for c, _ in pairs(clients) do
    pcall(c.read_stop, c)
    pcall(c.close, c)
  end
  clients = {}

  pcall(server.close, server)
  server = nil
  vim.notify("AgentBridge stopped", vim.log.levels.INFO)
  return { ok = true }
end

function M.setup(opts)
  M.config = vim.tbl_deep_extend("force", M.config, opts or {})

  vim.api.nvim_create_user_command("AgentBridgeStart", function()
    M.start({})
  end, {})

  vim.api.nvim_create_user_command("AgentBridgeStop", function()
    M.stop()
  end, {})
end

return M
