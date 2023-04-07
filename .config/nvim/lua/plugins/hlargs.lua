return {
    {
        "m-demare/hlargs.nvim",
        config = function()
            require("hlargs").setup()
        end,
        dependencies = { "nvim-treesitter/nvim-treesitter" },
        opts = {
            paint_catch_blocks = {
                declarations = true,
                usages = true,
            },
            extras = {
                named_parameters = true,
            },
        },
    },
}
