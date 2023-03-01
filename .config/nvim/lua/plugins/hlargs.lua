return {
  {
    "m-demare/hlargs.nvim",
    config = function()
      require("hlargs").setup()
    end,
    lazy = true,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
  },
}
