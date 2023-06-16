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
            { "paopaol/telescope-git-diffs.nvim" },
        },
        config = function()
            require("telescope").load_extension("undo")
            require("telescope").load_extension("file_browser")
            require("telescope").load_extension("git_diffs")
            require("telescope").setup({
                extensions = {
                    undo = {
                        -- telescope-undo.nvim config, see below
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
    {
        "aaronhallaert/advanced-git-search.nvim",
        config = function()
            -- optional: setup telescope before loading the extension
            require("telescope").setup({
                -- move this to the place where you call the telescope setup function
                extensions = {
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
                },
            })

            require("telescope").load_extension("advanced_git_search")
        end,
        dependencies = {
            "nvim-telescope/telescope.nvim",
            -- to show diff splits and open commits in browser
            "tpope/vim-rhubarb",
            -- OPTIONAL: to replace the diff from fugitive with diffview.nvim
            -- (fugitive is still needed to open in browser)
            -- "sindrets/diffview.nvim",
        },
    },
    {
        "adoyle-h/lsp-toggle.nvim",
        config = function()
            require("lsp-toggle").setup()
        end,
    },
}
