return {
    {
        'stevearc/oil.nvim',
        lazy = false,
        opts = {
            columns = {
                "icon",
                "permissions",
                "size",
                "mtime",
            }
        },
        -- Optional dependencies
        dependencies = { { "echasnovski/mini.icons", opts = {}, lazy = true } },

        keys = { { "<leader>.", "<cmd>Oil<cr>", desc = "file_browser" }, },

    }
}
