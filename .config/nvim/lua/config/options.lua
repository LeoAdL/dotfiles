-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

local opt = vim.opt

opt.completeopt = "menu,menuone,noselect"
opt.conceallevel = 2 -- Hide * markup for bold and italic, but not markers with substitutions
opt.confirm = true   -- Confirm to save changes before exiting modified buffer
opt.expandtab = true -- Use spaces instead of tabs
opt.fillchars = {
    foldopen = "",
    foldclose = "",
    fold = " ",
    foldsep = " ",
    diff = "╱",
    eob = " ",
}
opt.foldmethod = "expr"
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
opt.ignorecase = true -- Ignore case
opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize", "help", "globals", "skiprtp", "folds" }
opt.shortmess:append({ W = true, I = true, c = true, C = true })
opt.smoothscroll = true
opt.undolevels = 10000
opt.updatetime = 200      -- Save swap file and trigger CursorHold
opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode

-- Fix markdown indentation settings
vim.g.markdown_recommended_style = 0
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
opt.autowrite = true
opt.foldcolumn = "0"                                    -- '0' is not bad

opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard
opt.foldlevel = 999                                     -- Using ufo provider need a large value, feel free to decrease the value
opt.foldlevelstart = 999
opt.foldenable = true
vim.g.autoformat = true
-- Make line numbers default
opt.number = true
opt.relativenumber = true

-- Enable break indent
opt.smartindent = true -- Insert indents automatically

-- Save undo history
opt.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
opt.smartcase = true

-- Keep signcolumn on by default
opt.signcolumn = 'yes'

-- Decrease update time
opt.timeoutlen = 300

opt.showmode = false

-- NOTE: You should make sure your terminal supports this
opt.termguicolors = true

opt.tabstop = 4     -- A TAB character looks like 4 spaces
opt.softtabstop = 4 -- Number of spaces inserted instead of a TAB character
opt.shiftwidth = 4  -- Number of spaces inserted when indenting

opt.cursorline = false

opt.pumblend = 10
