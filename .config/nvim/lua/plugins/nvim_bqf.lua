return {
  { "kevinhwang91/nvim-bqf", ft = "qf", opts = { auto_resize_height = true, wrap = true } },
  {
    "junegunn/fzf",
    run = function()
      vim.fn["fzf#install"]()
    end,
  },
}
