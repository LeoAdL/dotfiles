local opt = vim.opt

-- General & Globals
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.markdown_recommended_style = 0                    -- Fix markdown indentation settings
opt.autowrite = true                                    -- Enable auto write
opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard safely

-- UI & Display
opt.number = true
opt.relativenumber = true
opt.signcolumn = "yes"
opt.showmode = false
opt.cursorline = false
opt.termguicolors = true
opt.conceallevel = 2      -- Hide * markup for bold and italic, but not markers with substitutions
opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode
opt.smoothscroll = true
opt.pumblend = 10

-- Tabs & Indentation
opt.tabstop = 4      -- A TAB character looks like 4 spaces
opt.softtabstop = 4  -- Number of spaces inserted instead of a TAB character
opt.shiftwidth = 4   -- Number of spaces inserted when indenting
opt.expandtab = true -- Use spaces instead of tabs
opt.smartindent = true

-- Folding
opt.foldenable = true
opt.foldmethod = "expr"
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
opt.foldcolumn = "0"
opt.foldlevel = 999 -- Keeps folds open by default
opt.foldlevelstart = 999
opt.fillchars = {
    foldopen = "",
    foldclose = "",
    fold = " ",
    foldsep = " ",
    diff = "╱",
    eob = " ",
}

-- Search
opt.ignorecase = true -- Ignore case
opt.smartcase = true  -- Case-insensitive searching UNLESS \C or capital in search

-- Behavior
opt.confirm = true   -- Confirm to save changes before exiting modified buffer
opt.undofile = true  -- Save undo history
opt.undolevels = 10000
opt.updatetime = 200 -- Save swap file and trigger CursorHold
opt.timeoutlen = 300 -- Decrease update time
opt.completeopt = "menu,menuone,noselect"
opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize", "help", "globals", "skiprtp", "folds" }
opt.shortmess:append({ W = true, I = true, c = true, C = true })
opt.splitbelow = true -- Put new windows below current
opt.splitright = true -- Put new windows right of current
