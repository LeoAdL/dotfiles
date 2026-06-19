-- Helper function to create augroups safely
local function augroup(name)
    return vim.api.nvim_create_augroup("custom_" .. name, { clear = true })
end

-- ==============================================================================
-- Autocommands
-- ==============================================================================

-- Check if we need to reload the file when it changed
vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
    group = augroup("checktime"),
    callback = function()
        if vim.o.buftype ~= "nofile" then
            vim.cmd("checktime")
        end
    end,
})

-- Highlight on yank
vim.api.nvim_create_autocmd("TextYankPost", {
    group = augroup("highlight_yank"),
    callback = function()
        vim.hl.on_yank()
    end,
})

-- Resize splits if window got resized
vim.api.nvim_create_autocmd({ "VimResized" }, {
    group = augroup("resize_splits"),
    callback = function()
        local current_tab = vim.fn.tabpagenr()
        vim.cmd("tabdo wincmd =")
        vim.cmd("tabnext " .. current_tab)
    end,
})

-- Close certain filetypes with <q>
vim.api.nvim_create_autocmd("FileType", {
    group = augroup("close_with_q"),
    pattern = {
        "PlenaryTestPopup",
        "checkhealth",
        "dbout",
        "gitsigns-blame",
        "grug-far",
        "help",
        "lspinfo",
        "neotest-output",
        "neotest-output-panel",
        "neotest-summary",
        "notify",
        "qf",
        "spectre_panel",
        "startuptime",
        "tsplayground",
    },
    callback = function(event)
        vim.bo[event.buf].buflisted = false
        vim.schedule(function()
            vim.keymap.set("n", "q", function()
                vim.cmd("close")
                pcall(vim.api.nvim_buf_delete, event.buf, { force = true })
            end, {
                buffer = event.buf,
                silent = true,
                desc = "Quit buffer",
            })
        end)
    end,
})

-- Auto create dir when saving a file, in case some intermediate directory does not exist
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    group = augroup("auto_create_dir"),
    callback = function(event)
        if event.match:match("^%w%w+:[\\/][\\/]") then
            return
        end
        local file = vim.uv.fs_realpath(event.match) or event.match
        vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
    end,
})

-- Specific indentation for Markdown
vim.api.nvim_create_autocmd("FileType", {
    group = augroup("markdown_indent"),
    pattern = "markdown",
    callback = function()
        vim.opt_local.shiftwidth = 2
    end,
})

-- ==============================================================================
-- Keymaps & Diagnostics
-- ==============================================================================

-- Global Diagnostic Mappings
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, { desc = "Show diagnostic error messages" })
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, { desc = "Open diagnostic loclist" })

-- ==============================================================================
-- LSP Autocommands & Keymaps
-- ==============================================================================

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', { clear = true }),
    callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- Helper function to easily map LSP keys with descriptions
        local map = function(keys, func, desc, mode)
            mode = mode or 'n'
            vim.keymap.set(mode, keys, func, { buffer = ev.buf, desc = "LSP: " .. desc })
        end

        map('K', vim.lsp.buf.hover, 'Hover Documentation')
        map('<C-k>', vim.lsp.buf.signature_help, 'Signature help')

        map('<space>D', vim.lsp.buf.type_definition, 'Type definition')
        map('<space>rn', vim.lsp.buf.rename, 'Rename')
        map('<space>ca', vim.lsp.buf.code_action, 'Code action', { 'n', 'v' })

        map('<space>wa', vim.lsp.buf.add_workspace_folder, 'Add workspace folder')
        map('<space>wr', vim.lsp.buf.remove_workspace_folder, 'Remove workspace folder')
        map('<space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, 'List workspace folders')

        map('<space>fF', function()
            vim.lsp.buf.format { async = true }
        end, 'Format buffer')
    end,
})
