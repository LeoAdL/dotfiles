-- Options first: leader must be set before lazy.nvim loads plugin keymaps
require("config.lazy")
require("config.options")

-- Setup lazy.nvim
require("lazy").setup({
    spec = {
        { import = "plugins" },
    },
    defaults = {
        lazy = false, -- plugins with keys/cmd/event/ft triggers are still lazy-loaded
        version = false, -- always use the latest git commit
    },
    checker = {
        enabled = true, -- check for plugin updates periodically
        notify = false, -- don't notify on update
    },
    performance = {
        rtp = {
            -- disable some rtp plugins
            disabled_plugins = {
                "gzip",
                -- "matchit",
                -- "matchparen",
                -- "netrwPlugin",
                "tarPlugin",
                "tutor",
                "zipPlugin",
            },
        },
    },
})
require("config.autocmds")
require("config.keymaps")
