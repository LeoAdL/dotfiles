-- ==============================================================================
-- General Keymaps
-- ==============================================================================

-- Plugin Manager
vim.keymap.set("n", "<leader>lL", "<cmd>Lazy<cr>", { desc = "Open lazy.nvim UI" })

-- Keep cursor centered when scrolling half pages
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll down half page and center" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll up half page and center" })

-- Clear search highlight (<C-l> used to do this; it now moves between windows)
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<cr><Esc>", { desc = "Clear search highlight" })

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
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostic error messages" })

-- Helper function for jumping between diagnostics
local diagnostic_goto = function(next, severity)
    severity = severity and vim.diagnostic.severity[severity] or nil
    return function()
        vim.diagnostic.jump({ count = next and 1 or -1, float = true, severity = severity })
    end
end

vim.keymap.set("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
vim.keymap.set("n", "[d", diagnostic_goto(false), { desc = "Prev Diagnostic" })
vim.keymap.set("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
vim.keymap.set("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev Error" })
vim.keymap.set("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
vim.keymap.set("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev Warning" })

-- ==============================================================================
-- Window navigation (pairs with the is_vim C-h/j/k/l bindings in tmux.conf)
-- ==============================================================================

-- Move to the neighbouring split, or hand off to tmux at the edge
local function navigate(dir)
    return function()
        local win = vim.api.nvim_get_current_win()
        vim.cmd.wincmd(dir)
        if vim.env.TMUX and win == vim.api.nvim_get_current_win() then
            vim.system({ "tmux", "select-pane", "-" .. ({ h = "L", j = "D", k = "U", l = "R" })[dir] })
        end
    end
end

for _, dir in ipairs({ "h", "j", "k", "l" }) do
    vim.keymap.set("n", "<C-" .. dir .. ">", navigate(dir), { desc = "Go to window " .. dir })
end
