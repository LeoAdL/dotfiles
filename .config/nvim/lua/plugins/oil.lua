return {
    {
        'stevearc/oil.nvim',
        event = { "BufReadPost", "BufWritePost", "BufNewFile" },
        opts = {
            columns = {
                "icon",
                "permissions",
                "size",
                "mtime",
            }
        },
        -- Optional dependencies
        dependencies = { { "echasnovski/mini.icons", opts = {} } },
    }
}
