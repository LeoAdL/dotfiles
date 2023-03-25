return {
  {
    "jghauser/papis.nvim",
    enabled = false,
    requires = {
      "kkharji/sqlite.lua",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function(plugin)
      vim.opt.rtp:append(plugin.dir .. "/opt/homebrew/Cellar/libyaml")
      require("papis").setup({
        papis_python = {
          dir = "~/Library/papers/",
          info_name = "info.yaml", -- (when setting papis options `-` is replaced with `_`
          -- in the keys names)
          notes_name = [[notes.norg]],
        },
        -- Enable the default keymaps
        enable_keymaps = true,
        cite_formats = {
          tex = { "\\citet{%s}", "\\citet[tp]?%*?{%s}" },
          markdown = "@%s",
          rmd = "@%s",
          plain = "%s",
          org = { "[cite:@%s]", "%[cite:@%s]" },
        },
      })
    end,
    build = ":PapisStart",
  },
}
