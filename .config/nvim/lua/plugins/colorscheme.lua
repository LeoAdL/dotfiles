return {
    {
        "catppuccin/nvim",
        lazy = false,
        priority = 1000,
        name = "catppuccin", -- Crucial: Without this, lazy.nvim would name the plugin "nvim"
        opts = {
            -- integrations for installed plugins are detected automatically (auto_integrations)
            lsp_styles = {
                underlines = {
                    errors = { "undercurl" },
                    hints = { "undercurl" },
                    warnings = { "undercurl" },
                    information = { "undercurl" },
                },
            },
        },
        config = function(_, opts)
            require("catppuccin").setup(opts)
            vim.cmd.colorscheme("catppuccin")
        end,
    },
    {
        "nvim-lualine/lualine.nvim",
        event = "VeryLazy",
        opts = {
            sections = {
                lualine_y = { "location" },
                lualine_z = { "progress" },
            },
        },
    },
    {
        -- Icon provider; also stands in for nvim-web-devicons (used by lualine, trouble, ...)
        "nvim-mini/mini.icons",
        lazy = true,
        opts = {},
        init = function()
            package.preload["nvim-web-devicons"] = function()
                require("mini.icons").mock_nvim_web_devicons()
                return package.loaded["nvim-web-devicons"]
            end
        end,
    },
}
