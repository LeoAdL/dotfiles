return {
    {
        "NeogitOrg/neogit",
        lazy = true,
        dependencies = {
            "nvim-lua/plenary.nvim", -- required
        },
        keys = {
            { "<leader>gg", "<cmd>Neogit<cr>", desc = "NeoGit" }
        },
    },
}
