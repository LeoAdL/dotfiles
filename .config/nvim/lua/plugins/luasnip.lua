return {
    {
        "L3MON4D3/LuaSnip",
        lazy = true,
        build = "make install_jsregexp",
        config = function(_, opts)
            require("luasnip").setup(opts)
            require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.stdpath("config") .. "/lua/snippets" } })
        end,
        opts = {
            history = true,
            delete_check_events = "TextChanged",
        },
    },
}
