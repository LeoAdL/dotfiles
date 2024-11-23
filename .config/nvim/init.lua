require("config.lazy")
require("config/options")
require("config/autocmds")
require("config/keymaps")
-- Setup lazy.nvim
require("lazy").setup({
    spec = {
        -- import your plugins
        { import = "plugins" },
    },
    -- Configure any other settings here. See the documentation for more details.
    -- colorscheme that will be used when installing plugins.
    -- automatically check for plugin updates
    checker = { enabled = true },
})

vim.cmd.colorscheme "catppuccin"
vim.cmd.set "shortmess=I"
