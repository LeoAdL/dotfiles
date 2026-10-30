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
        dependencies = { "nvim-mini/mini.icons" },

        keys = { { "<leader>.", "<cmd>Oil<cr>", desc = "file_browser" }, },

    }
}
