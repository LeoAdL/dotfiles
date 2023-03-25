return {
  {
    "telescope.nvim",
    enabled = true,
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        config = function()
          require("telescope").load_extension("fzf")
        end,
      },
      { "nvim-telescope/telescope-file-browser.nvim" },
      { "debugloop/telescope-undo.nvim" },
    },
    config = function()
      require("telescope").load_extension("undo")
      require("telescope").load_extension("file_browser")
      require("telescope").setup({
        extensions = {
          undo = {
            -- telescope-undo.nvim config, see below
          },
        },
      })
    end,
    mappings = {
      i = {
        ["<C-n>"] = require("telescope.actions").cycle_history_next,
        ["<C-p>"] = require("telescope.actions").cycle_history_prev,
        ["<C-j>"] = require("telescope.actions").move_selection_next,
        ["<C-k>"] = require("telescope.actions").move_selection_previous,
      },
      n = { ["q"] = require("telescope.actions").close },
    },
    keys = {
      { "<leader>tt", "<cmd>Telescope<cr>", desc = "Open Telescope" },
    },
  },
}
