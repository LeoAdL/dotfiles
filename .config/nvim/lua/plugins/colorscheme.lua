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
        fidget = true,
        mason = true,
        neogit = true,
        treesitter_context = true,
        which_key = true,
      },
    }
  },
  {
    "nvim-lualine/lualine.nvim",
    opts = {
      theme = "catppuccin",
      sections = {
        lualine_y = {
          "location"
        },
        lualine_z = {
          "progress"
        },
      }
    }
  },
  { 'rose-pine/neovim', name = 'rose-pine', enabled = false },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },
}
