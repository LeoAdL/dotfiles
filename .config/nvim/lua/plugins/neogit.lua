return {
    {
        "NeogitOrg/neogit",
        event = { "BufReadPost", "BufWritePost", "BufNewFile" },
        dependencies = {
            "nvim-lua/plenary.nvim",  -- required
            "sindrets/diffview.nvim", -- optional - Diff integration

            -- Only one of these is needed, not both.
            "nvim-telescope/telescope.nvim", -- optional
            -- "ibhagwan/fzf-lua",              -- optional
        },
        opt = {
            integrations = {
                -- If enabled, use telescope for menu selection rather than vim.ui.select.
                -- Allows multi-select and some things that vim.ui.select doesn't.
                -- Neogit only provides inline diffs. If you want a more traditional way to look at diffs, you can use `diffview`.
                -- The diffview integration enables the diff popup.
                --
                -- Requires you to have `sindrets/diffview.nvim` installed.
                diffview = true,

                -- If enabled, uses fzf-lua for menu selection. If the telescope integration
                -- is also selected then telescope is used instead
                -- Requires you to have `ibhagwan/fzf-lua` installed.
                telescope = true,
            },
        },
    },
}
