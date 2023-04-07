return {
    {
        "stevearc/oil.nvim",
        config = function()
            vim.g.loaded_netrw = 1
            vim.g.loaded_netrwPlugin = 1

            require("oil").setup({
                columns = {
                    "icon",
                    "size",
                    "mtime",
                },
            })
        end,
    },
}
