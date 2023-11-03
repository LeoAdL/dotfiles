return {
  {
    "L3MON4D3/LuaSnip",
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load({ paths = { "/Users/leoap/.config/nvim/lua/snippets" } })
    end,
  },
  { "rafamadriz/friendly-snippets", enabled = false },
}
