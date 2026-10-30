require("config.lazy")
require("config/options")
-- Setup lazy.nvim

require("lazy").setup({
    spec = {
        { import = "plugins" },
    },
    defaults = {
        lazy = false,    -- plugins with keys/cmd/event/ft triggers are still lazy-loaded
        version = false, -- always use the latest git commit
    },
    rocks = { enabled = false }, -- no plugin needs luarocks
    checker = {
        enabled = true, -- check for plugin updates periodically
        notify = false, -- notify on update
    },                  -- automatically check for plugin updates
    performance = {
        rtp = {
            -- disable some rtp plugins
            disabled_plugins = {
                "gzip",
                -- "matchit",
                -- "matchparen",
                "netrwPlugin", -- replaced by oil.nvim
                "tarPlugin",
                "tohtml",
                "tutor",
                "zipPlugin",
            },
        },
    },
})
require("config/autocmds")
require("config/keymaps")

