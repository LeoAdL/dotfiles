return {
  {
    "aserowy/tmux.nvim",
    config = function()
      return require("tmux").setup({ enable_default_keybindings = true })
    end,
  },
}
