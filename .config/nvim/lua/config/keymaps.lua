vim.keymap.set("n", "<C-h>", "<cmd>SmartCursorMoveLeft<cr>", { desc = "Go to left window" })
vim.keymap.set("n", "<C-j>", "<cmd>SmartCursorMoveDown<cr>", { desc = "Go to lower window" })
vim.keymap.set("n", "<C-k>", "<cmd>SmartCursorMoveUp<cr>", { desc = "Go to upper window" })
vim.keymap.set("n", "<C-l>", "<cmd>SmartCursorMoveRight<cr>", { desc = "Go to right window" })
vim.keymap.set("n", "<leader>e", "<cmd>NnnPicker<cr>", { desc = "file_browser" })
vim.keymap.set("n", "<leader>lL", "<cmd>Lazy<cr>", { desc = "Lazy" })
vim.keymap.set("n", "<leader>xx", "<cmd>Telescope diagnostics<cr>", { desc = "Diagnostics" })
vim.keymap.set("n", "<leader>xg", "<cmd>Gitsigns setqflist<cr>", { desc = "Git QuickFix" })
vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<cr>", { desc = "NeoGit" })
vim.keymap.set("n", "<leader>ss", "<cmd>SymbolsOutline<cr>", { desc = "SymbolsOutline" })

-- open file_browser with the path of the current buffer
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- lazy
--
-- better indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")
-- diagnostic
local diagnostic_goto = function(next, severity)
    local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
    severity = severity and vim.diagnostic.severity[severity] or nil
    return function()
        go({ severity = severity })
    end
end
vim.keymap.set("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
vim.keymap.set("n", "[d", diagnostic_goto(false), { desc = "Prev Diagnostic" })
vim.keymap.set("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
vim.keymap.set("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev Error" })
vim.keymap.set("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
vim.keymap.set("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev Warning" })
-- Global mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "previous diagnostic" })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "next diagnostic" })
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, { desc = "LSP loclist" })

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- Buffer local mappings.
        -- See `:help vim.lsp.*` for documentation on any of the below functions
        local opts = { buffer = ev.buf }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer = ev.buf, desc = "Goto declaration" })
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = ev.buf, desc = "Goto definition" })
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = ev.buf, desc = "Hover" })
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { buffer = ev.buf, desc = "Goto implementation" })
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, { buffer = ev.buf, desc = "signature_help" })
        vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder,
            { buffer = ev.buf, desc = "add_workspace_folder" })
        vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder,
            { buffer = ev.buf, desc = "remove_workspace_folder" })
        vim.keymap.set('n', '<space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, { buffer = ev.buf, desc = "list_workspace_folders" })
        vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, { buffer = ev.buf, desc = "type_definition" })
        vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, { buffer = ev.buf, desc = "rename" })
        vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, { buffer = ev.buf, desc = "code_action" })
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, { buffer = ev.buf, desc = "Goto references" })
        vim.keymap.set('n', '<space>fF', function()
            vim.lsp.buf.format { async = true }
        end, { buffer = ev.buf, desc = "LSP format" })
    end,
})
