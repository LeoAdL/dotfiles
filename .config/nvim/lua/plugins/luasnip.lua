return {
    {
        "L3MON4D3/LuaSnip",
        config = function()
            require("luasnip.loaders.from_vscode").lazy_load({ paths = { "/Users/leoap/.config/nvim/lua/snippets" } })
        end,
        keys = function()
            return {}
        end,
        opts = {
            history = true,
            delete_check_events = "TextChanged",
        },
    },
    { "rafamadriz/friendly-snippets", enabled = false },
}
