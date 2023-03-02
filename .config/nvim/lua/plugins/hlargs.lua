return {
  {
    "m-demare/hlargs.nvim",
    config = function()
      require("hlargs").setup()
    end,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
  },
}
