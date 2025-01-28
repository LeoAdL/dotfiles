-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.g.root_spec = { "lsp", { ".git", "lua" }, "cwd" }
vim.opt.conceallevel = 2 -- hide * markup for bold and italic
vim.opt.autowrite = true
vim.opt.autochdir = true
vim.opt.confirm = true                                      -- Confirm to save changes before exiting modified buffer
vim.opt.foldcolumn = "1"                                    -- '0' is not bad

vim.opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard
vim.opt.foldlevel = 999                                     -- Using ufo provider need a large value, feel free to decrease the value
vim.opt.foldlevelstart = 999
vim.opt.foldenable = true
vim.opt.foldcolumn = '0'
vim.g.autoformat = true
-- Make line numbers default
vim.opt.number = true
vim.opt.relativenumber = true

-- Enable break indent
vim.opt.breakindent = true
vim.opt.smartindent = true -- Insert indents automatically

-- Save undo history
vim.opt.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = 'yes'

-- Decrease update time
vim.opt.updatetime = 250
vim.opt.timeoutlen = 300


vim.opt.splitright = true
vim.opt.splitbelow = true
-- Set completeopt to have a better completion experience
vim.opt.completeopt = 'menuone,noselect'
vim.opt.cursorline = true
vim.opt.cursorlineopt = "number"
vim.opt.showmode = false

vim.opt.mouse = 'a'

-- NOTE: You should make sure your terminal supports this
vim.opt.termguicolors = true

vim.opt.tabstop = 4      -- A TAB character looks like 4 spaces
vim.opt.expandtab = true -- Pressing the TAB key will insert spaces instead of a TAB character
vim.opt.softtabstop = 4  -- Number of spaces inserted instead of a TAB character
vim.opt.shiftwidth = 4   -- Number of spaces inserted when indenting
