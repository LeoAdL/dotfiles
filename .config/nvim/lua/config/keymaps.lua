-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
-- Move to window using the <ctrl> hjkl keys

local function map(mode, lhs, rhs, opts)
  local keys = require("lazy.core.handler").handlers.keys
  ---@cast keys LazyKeysHandler
  -- do not create the keymap if a lazy keys handler exists
  if not keys.active[keys.parse({ lhs, mode = mode }).id] then
    opts = opts or {}
    opts.silent = opts.silent ~= false
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

map("n", "<C-h>", "<cmd>SmartCursorMoveLeft<cr>", { desc = "Go to left window" })
map("n", "<C-j>", "<cmd>SmartCursorMoveDown<cr>", { desc = "Go to lower window" })
map("n", "<C-k>", "<cmd>SmartCursorMoveUp<cr>", { desc = "Go to upper window" })
map("n", "<C-l>", "<cmd>SmartCursorMoveRight<cr>", { desc = "Go to right window" })
map("n", "<leader>e", "<cmd>Telescope file_browser<cr>", { desc = "file_browser" })
map("n", "<leader>e", "<cmd>lua require('ranger-nvim').open()<cr>", { desc = "ranger" })
map("n", "<leader>lv", "<cmd>TexlabForward<cr>", { desc = "forward search" })
map("n", "<leader>lC", "<cmd>silent ! latexmk -c <cr>", { desc = "Clean Files" })
map("n", "<leader>ll", "<cmd>Lazy<cr>", { desc = "Lazy" })
map("n", "<leader>xx", "<cmd>lua vim.diagnostic.setqflist()<cr>", { desc = "QuickFix" })
map("n", "<leader>xg", "<cmd>Gitsigns setqflist<cr>", { desc = "Git QuickFix" })

-- open file_browser with the path of the current buffer
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- lazy
--
