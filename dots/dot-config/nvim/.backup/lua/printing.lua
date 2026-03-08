local M = {}

function M.print_friendly_reduced()
    -- 1. Setup Bold-Only Black & White environment
    vim.cmd('set background=light')
    vim.cmd('syntax off')
    vim.cmd('hi clear')
    
    -- Force common code elements to be Bold and Black
    local bold_black = { bold = true, fg = "#000000" }
    local groups = {"Statement", "Type", "Identifier", "Keyword", "PreProc"}
    for _, group in ipairs(groups) do
        vim.api.nvim_set_hl(0, group, bold_black)
    end
    
    -- 2. Define Paths for the LOCAL folder
    local local_dir = vim.fn.expand('%:p:h')   -- Full path to the folder containing the file
    local base_name = vim.fn.expand('%:t:r')   -- Filename without extension
    local html_file = local_dir .. "/temp_print.html"
    local pdf_file = local_dir .. "/" .. base_name .. ".pdf"
    local tmp_pdf = local_dir .. "/tmp_uncompressed.pdf"

    -- 3. Export to HTML
    vim.cmd('TOhtml')
    vim.cmd('w! ' .. vim.fn.fnameescape(html_file))
    vim.cmd('bwipeout!') 
    
    -- 4. macOS: Convert HTML -> PDF -> Compressed PDF in local folder
    -- Use single quotes in the shell command to handle spaces in paths
    local cmd = string.format(
        "cupsfilter '%s' > '%s' 2>/dev/null && " ..
        "gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook " ..
        "-dNOPAUSE -dQUIET -dBATCH -sOutputFile='%s' '%s' && " ..
        "rm '%s' '%s' && open '%s'",
        html_file, tmp_pdf, pdf_path or pdf_file, tmp_pdf, html_file, tmp_pdf, pdf_file
    )
    
    os.execute(cmd)
    
    -- 5. Restore original state
    vim.cmd('syntax on')
    print("Reduced PDF saved in local folder: " .. pdf_file)
end

return M
