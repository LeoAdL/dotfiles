return {
  -- add gruvbox
  -- Configure LazyVim to load gruvbox
  {
    "catppuccin/nvim",
    enabled = true,
    name = "catppuccin",
    priority = 1000,
    opts = {
      integrations = {
        cmp = true,
        gitsigns = true,
        nvimtree = true,
        treesitter = true,
        notify = true,
        mini = true,
        beacon = true,
        fidget = true,
        mason = true,
        neogit = true,
        navic = {
          enabled = true,
          custom_bg = "NONE", -- "lualine" will set background to mantle
        },
        treesitter_context = true,
        symbols_outline = true,
        which_key = true,
      },
    }
  },
  { "nvim-lualine/lualine.nvim", opts = { theme = "catppuccin" } },
  { 'rose-pine/neovim',          name = 'rose-pine',             enabled = false },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },
}
