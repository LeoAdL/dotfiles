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
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-file-browser.nvim" },
      { "debugloop/telescope-undo.nvim" },
    },
    config = function()
      local ts = require("telescope")
      local tsu = require("telescope-undo.actions")
      ts.setup({
        extensions = {
          undo = {
            use_delta = true,
            side_by_side = false,
            layout_config = {
              preview_height = 0.8,
            },
            mappings = {
              i = {
                ["<cr>"] = tsu.restore,
                ["<C-d>"] = tsu.yank_deletions,
                ["<C-a>"] = tsu.yank_additions,
              },
              n = {
                ["<cr>"] = tsu.yank_additions,
                ["<C-d>"] = tsu.yank_deletions,
                ["<C-a>"] = tsu.restore,
              },
            },
          },
          file_browser = {
            -- path
            -- cwd
            cwd_to_path = false,
            grouped = false,
            files = true,
            add_dirs = true,
            depth = 1,
            auto_depth = false,
            select_buffer = true,
            hidden = true,
            hijack_netrw = true,
          },
        },
      })
      require("telescope").load_extension("file_browser")
      require("telescope").load_extension("undo")
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
      {
        "<leader>su",
        ":Telescope undo<cr>",
        desc = "Undo history",
      },
    },
  },
  {
    "adoyle-h/lsp-toggle.nvim",
    config = function()
      require("lsp-toggle").setup()
    end,
  },
}
