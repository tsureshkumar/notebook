local function Capitalize(str)
  str = string.lower(str)
  return (string.gsub(str, '^%l', string.upper))
end

local function CamelCase(str)
  strs = (str..'_'):gmatch("(%w+)_")
  res = ""
  for word in strs
  do
    res = res .. Capitalize(word)
  end
  return res
end

local function camelCase(str)
  str = CamelCase(str)
  return (string.gsub(str, '^.', string.lower))
end

local function CamelCaseVim()
  local line_number = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_get_current_line()
  local col = vim.api.nvim_win_get_cursor(0)[2] + 1

  local start_pos = col
  while start_pos > 1 and line:sub(start_pos - 1, start_pos - 1):match("%a") do
    start_pos = start_pos - 1
  end

  local end_pos = col
  while end_pos <= #line and line:sub(end_pos, end_pos):match("%a") do
    end_pos = end_pos + 1
  end

  local word = line:sub(start_pos, end_pos - 1)
  local camel_word = word:gsub("(%a)(%a*)", function(first, rest)
    return first:upper() .. rest:lower()
  end)

  -- Modify the buffer contents to replace the current word with the camel case version
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_text(buf, line_number - 1, start_pos - 1, line_number - 1, end_pos - 1, { camel_word })

  -- Adjust the cursor position
  local new_col = start_pos + #camel_word - 1
  vim.api.nvim_win_set_cursor(0, { line_number, new_col })
end

local M = {}

function M.CamelCase()
  pcall(CamelCaseVim)
end

-- return M
--
local vimfn = vim.fn

local function switch_case()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  local word = vimfn.expand('<cword>')
  local word_start = vimfn.matchstrpos(vimfn.getline('.'), '\\k*\\%' .. (col+1) .. 'c\\k*')[2]

  -- Detect camelCase
  if word:find('[a-z][A-Z]') then
    -- Convert camelCase to snake_case
    local snake_case_word = word:gsub('([a-z])([A-Z])', '%1_%2'):lower()
    vim.api.nvim_buf_set_text(0, line - 1, word_start, line - 1, word_start + #word, {snake_case_word})
  -- Detect snake_case
  elseif word:find('_[a-z]') then
    -- Convert snake_case to camelCase
    local camel_case_word = word:gsub('(_)([a-z])', function(_, l) return l:upper() end)
    vim.api.nvim_buf_set_text(0, line - 1, word_start, line - 1, word_start + #word, {camel_case_word})
  else
    print("Not a snake_case or camelCase word")
  end
end

return { switch_case = switch_case }
