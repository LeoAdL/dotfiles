-- ==============================================================================
-- General Keymaps
-- ==============================================================================

-- Plugin Manager
vim.keymap.set("n", "<leader>lL", "<cmd>Lazy<cr>", { desc = "Open lazy.nvim UI" })

-- Keep cursor centered when scrolling half pages
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll down half page and center" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll up half page and center" })

-- Keep cursor centered when navigating search results
vim.keymap.set("n", "n", "nzzzv", { desc = "Next search result and center" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Prev search result and center" })

-- Better indenting in visual mode (keeps selection active after indenting)
vim.keymap.set("v", "<", "<gv", { desc = "Indent left and reselect" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent right and reselect" })

-- ==============================================================================
-- Diagnostics
-- ==============================================================================

-- Show line diagnostics
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Line Diagnostics" })

-- Helper function for jumping between diagnostics
local diagnostic_goto = function(next, severity)
    local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
    severity = severity and vim.diagnostic.severity[severity] or nil
    return function()
        go({ severity = severity })
    end
end

vim.keymap.set("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
vim.keymap.set("n", "[d", diagnostic_goto(false), { desc = "Prev Diagnostic" })
vim.keymap.set("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
vim.keymap.set("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev Error" })
vim.keymap.set("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
vim.keymap.set("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev Warning" })

-- ==============================================================================
-- LSP Keymaps
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

        map('gD', vim.lsp.buf.declaration, 'Goto declaration')
        map('gd', vim.lsp.buf.definition, 'Goto definition')
        map('gi', vim.lsp.buf.implementation, 'Goto implementation')
        map('gr', vim.lsp.buf.references, 'Goto references')
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
