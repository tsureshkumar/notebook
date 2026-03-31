local M = {}

local function capitalize(str)
    str = string.lower(str)
    return (string.gsub(str, "^%l", string.upper))
end

local function camel_case(str)
    local res = ""
    for word in (str .. "_"):gmatch("(%w+)_") do
        res = res .. capitalize(word)
    end
    return res
end

local function camel_case_vim()
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

    local buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_text(buf, line_number - 1, start_pos - 1, line_number - 1, end_pos - 1, { camel_word })
    vim.api.nvim_win_set_cursor(0, { line_number, start_pos + #camel_word - 1 })
end

function M.camel_case_word()
    pcall(camel_case_vim)
end

local function switch_case()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    local word = vim.fn.expand("<cword>")
    local word_start = vim.fn.matchstrpos(vim.fn.getline("."), "\\k*\\%" .. (col + 1) .. "c\\k*")[2]

    if word:find("[a-z][A-Z]") then
        local snake = word:gsub("([a-z])([A-Z])", "%1_%2"):lower()
        vim.api.nvim_buf_set_text(0, line - 1, word_start, line - 1, word_start + #word, { snake })
    elseif word:find("_[a-z]") then
        local camel = word:gsub("(_)([a-z])", function(_, l) return l:upper() end)
        vim.api.nvim_buf_set_text(0, line - 1, word_start, line - 1, word_start + #word, { camel })
    end
end

M.switch_case = switch_case

return M
