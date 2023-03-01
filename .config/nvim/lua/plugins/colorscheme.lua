return {
  -- add gruvbox
  {
    "gbprod/nord.nvim",
  },
  { "nvim-lualine/lualine.nvim", opts = { theme = "nord" } },
  -- Configure LazyVim to load gruvbox
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "nord",
    },
  },
}
