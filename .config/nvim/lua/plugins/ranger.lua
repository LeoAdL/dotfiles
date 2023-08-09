return {
  {
    "kelly-lin/ranger.nvim",
    enabled = false,
    config = function()
      require("ranger-nvim").setup({ replace_netrw = true })
    end,
  },
}
