return {
    {
        "nvim-neorg/neorg",
        build = ":Neorg sync-parsers",
        opts = {
            load = {
                ["core.defaults"] = {}, -- Loads default behaviour
                ["core.concealer"] = {}, -- Adds pretty icons to your documents
                ["core.qol.toc"] = {},
                ["core.completion"] = { config = { engine = "nvim-cmp" } },
                ["core.integrations.telescope"] = {},
                ["core.dirman"] = { -- Manages Neorg workspaces
                    config = {
                        workspaces = {
                            notes = "~/org/notes",
                        },
                    },
                },
            },
        },
        dependencies = { { "nvim-lua/plenary.nvim" }, { "nvim-neorg/neorg-telescope" } },
    },
}
