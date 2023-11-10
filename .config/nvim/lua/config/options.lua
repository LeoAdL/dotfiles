-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.opt.conceallevel = 2       -- hide * markup for bold and italic
vim.opt.autowrite = true
vim.opt.confirm = true         -- Confirm to save changes before exiting modified buffer
vim.opt.inccommand = "nosplit" -- preview incremental substitute

vim.opt.autochdir = true
vim.opt.wrap = true
vim.o.foldcolumn = "1" -- '0' is not bad
vim.o.foldlevel = 999  -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 999
vim.o.foldenable = true
vim.o.foldcolumn = '0'
vim.g.autoformat = true
-- Make line numbers default
vim.wo.number = true


-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true
vim.opt.smartindent = true -- Insert indents automatically

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

vim.wo.relativenumber = true
vim.o.tabstop = 4      -- A TAB character looks like 4 spaces
vim.o.expandtab = true -- Pressing the TAB key will insert spaces instead of a TAB character
vim.o.softtabstop = 4  -- Number of spaces inserted instead of a TAB character
vim.o.shiftwidth = 4   -- Number of spaces inserted when indenting

local home = os.getenv("HOME")

local path = table.concat({
    "/usr/share/lua/5.1/?.lua",
    "/usr/share/lua/5.1/?/init.lua",
    "/usr/lib/lua/5.1/?.lua",
    "/usr/lib/lua/5.1/?/init.lua",
    "./?.lua",
    "./?/init.lua",
    "~/.luarocks/share/lua/5.1/?.lua",
    "~/.luarocks/share/lua/5.1/?/init.lua",
}, ";")

local cpath = table.concat({
    "/usr/lib/lua/5.1/?.so",
    "/usr/lib/lua/5.1/loadall.so",
    "./?.so",
    "~/.luarocks/lib/lua/5.1/?.so",
}, ";")

package.path = path:gsub("~", home)
package.cpath = cpath:gsub("~", home)
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
})
