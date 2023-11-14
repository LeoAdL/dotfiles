return {
    {
        "catppuccin/nvim",
        enabled = true,
        name = "catppuccin",
        priority = 1000,
        opts = {
            integrations = {
                cmp = true,
                gitsigns = true,
                treesitter = true,
                notify = true,
                mini = true,
                fidget = true,
                neogit = true,
                treesitter_context = true,
                which_key = true,
            },
        }
    },
    {
        "nvim-lualine/lualine.nvim",
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
}
