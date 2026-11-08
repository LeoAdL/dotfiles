return {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
        spec = {
            { "<leader>c", group = "code" },
            { "<leader>f", group = "find" },
            { "<leader>g", group = "git" },
            { "<leader>gh", group = "hunks" },
            { "<leader>l", group = "lazy" },
            { "<leader>q", group = "session" },
            { "<leader>r", group = "refactor" },
            { "<leader>s", group = "search" },
            { "<leader>t", group = "pickers" },
            { "<leader>u", group = "ui" },
            { "<leader>w", group = "workspace" },
            { "<leader>x", group = "diagnostics/quickfix" },
        },
    },
}
