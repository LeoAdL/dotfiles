return {
  { "echasnovski/mini.nvim#minisurround", enabled = false },
  {
    "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end,
  },
}
