return {
    {
        'stevearc/oil.nvim',
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
    }
}
