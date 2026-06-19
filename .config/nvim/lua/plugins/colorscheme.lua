return {
    {
        "catppuccin/nvim",
        lazy = false,
        priority = 1000,
        name = "catppuccin", -- Crucial: Without this, lazy.nvim would name the plugin "nvim"
        opts = {
            compile_path = vim.fn.stdpath("cache") .. "/catppuccin",
            compile = {
                enabled = true,
                compiler = "cobalt",
            },
            lsp_styles = {
                underlines = {
                    errors = { "undercurl" },
                    hints = { "undercurl" },
                    warnings = { "undercurl" },
                    information = { "undercurl" },
                },
            },
            integrations = {
                blink_cmp = true,
                flash = true,
                grug_far = true,
                gitsigns = true,
                headlines = true,
                illuminate = true,
                indent_blankline = { enabled = true },
                lsp_trouble = true,
                mason = true,
                markdown = true,
                mini = true,
                navic = { enabled = true, custom_bg = "lualine" },
                neotest = true,
                noice = true,
                notify = true,
                semantic_tokens = true,
                snacks = true,
                treesitter = true,
                treesitter_context = true,
                which_key = true,
            },
        },
        config = function(_, opts)
            -- Apply the configuration
            require("catppuccin").setup(opts)

            -- Actually turn the colorscheme on
            vim.cmd.colorscheme("catppuccin")
        end,
    },
    {
        "nvim-lualine/lualine.nvim",
        event = "VeryLazy",
        opts = {
            theme = "catppuccin",
            sections = {
                lualine_y = {
                    "location"
                },
                lualine_z = {
                    "progress"
                },
            }
        }
    },
    {
        "gbprod/nord.nvim",
        lazy = false,
        enabled = false,
        priority = 1000,
        config = function()
            require("nord").setup({})
            vim.cmd.colorscheme("nord")
        end,
    },
}
