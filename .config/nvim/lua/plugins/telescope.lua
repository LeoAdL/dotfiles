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
      { "nvim-telescope/telescope-file-browser.nvim", enabled = false },
      { "debugloop/telescope-undo.nvim" },
      { "paopaol/telescope-git-diffs.nvim" },
      { "aaronhallaert/advanced-git-search.nvim" },
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
                ["<cr>"] = tsu.yank_additions,
                ["<C-d>"] = tsu.yank_deletions,
                ["<C-a>"] = tsu.restore,
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
            auto_depth = true,
            select_buffer = true,
            hidden = true,
            hijack_netrw = true,
          },
        },
        advanced_git_search = {
          -- fugitive or diffview
          diff_plugin = "fugitive",
          -- customize git in previewer
          -- e.g. flags such as { "--no-pager" }, or { "-c", "delta.side-by-side=false" }
          git_flags = {},
          -- customize git diff in previewer
          -- e.g. flags such as { "--raw" }
          git_diff_flags = {},
          -- Show builtin git pickers when executing "show_custom_functions" or :AdvancedGitSearch
          show_builtin_git_pickers = false,
        },
      })
      require("telescope").load_extension("undo")
      require("telescope").load_extension("git_diffs")
      require("telescope").load_extension("advanced_git_search")
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
