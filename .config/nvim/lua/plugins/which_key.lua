return {
    'folke/which-key.nvim',
    event = "VeryLazy",
    opts = {
        spec = {
            { "<leader>c", group = "code" },
            { "<leader>f", group = "find" },
            { "<leader>g", group = "git" },
            { "<leader>gh", group = "hunks" },
            { "<leader>q", group = "session" },
            { "<leader>s", group = "search" },
            { "<leader>u", group = "ui" },
            { "<leader>w", group = "workspace" },
            { "<leader>x", group = "diagnostics/quickfix" },
        },
    },
}
