vim.keymap.set("n", "<leader>lL", "<cmd>Lazy<cr>", { desc = "Lazy" })
local bufopts = { noremap = true, silent = true, desc = "Expand LSP diagnostic" }

vim.keymap.set("n", "<space>e", vim.diagnostic.open_float, bufopts)

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
-- load the session for the current directory
vim.keymap.set("n", "<leader>qs", function() require("persistence").load() end, { desc = "cwd session" })

-- select a session to load
vim.keymap.set("n", "<leader>qS", function() require("persistence").select() end, { desc = "select session" })

-- load the last session
vim.keymap.set("n", "<leader>ql", function() require("persistence").load({ last = true }) end, { desc = "last session" })

-- stop Persistence => session won't be saved on exit
vim.keymap.set("n", "<leader>qd", function() require("persistence").stop() end, { desc = "stop session" })
