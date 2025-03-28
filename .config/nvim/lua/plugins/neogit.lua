return {
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim", -- required
        },
        keys = {
            { "<leader>gg", "<cmd>Neogit<cr>", desc = "NeoGit" }
        },
        opt = {
            integrations = {
                -- If enabled, use telescope for menu selection rather than vim.ui.select.
                -- Allows multi-select and some things that vim.ui.select doesn't.
                -- Neogit only provides inline diffs. If you want a more traditional way to look at diffs, you can use `diffview`.
                -- The diffview integration enables the diff popup.
                --
                -- Requires you to have `sindrets/diffview.nvim` installed.

            },
        },
    },
}
