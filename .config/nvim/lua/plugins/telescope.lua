return {
    {
        "telescope.nvim",
        dependencies = {
            {
                "nvim-telescope/telescope-fzf-native.nvim",
                build = "make",
                config = function()
                    require("telescope").load_extension("fzf")
                end,
            },
            { "nvim-lua/plenary.nvim" },
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
    {
        "ibhagwan/fzf-lua",
        -- optional for icon support
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            -- calling `setup` is optional for customization
            require("fzf-lua").setup({ "fzf-native" })
            require("fzf-lua").register_ui_select()
        end,
        keys = { { "<leader>tt", "<cmd>FzfLua<cr>",              desc = "Open FzfLua" },
            {
                "<leader>,",
                "<cmd>FzfLua buffers sort_mru=true sort_lastused=true<cr>",
                desc = "Switch Buffer",
            },
            { "<leader>:",  "<cmd>FzfLua command_history<cr>",       desc = "Command History" },
            { "<leader>fb", "<cmd>FzfLua buffers<cr>",               desc = "Buffers" },
            { "<leader>ff", "<cmd>FzfLua files<cr>",                 desc = "Find Files (root dir)" },
            { "<leader>fr", "<cmd>FzfLua oldfiles<cr>",              desc = "Recent" },
            -- git
            { "<leader>gc", "<cmd>FzfLua git_commits<CR>",           desc = "commits" },
            { "<leader>/",  "<cmd>FzfLua live_grep_native<CR>",      desc = "live grep" },
            { "<leader>gs", "<cmd>FzfLua git_status<CR>",            desc = "status" },
            -- search
            { '<leader>s"', "<cmd>FzfLua registers<cr>",             desc = "Registers" },
            { "<leader>sa", "<cmd>FzfLua autocommands<cr>",          desc = "Auto Commands" },
            { "<leader>sb", "<cmd>FzfLua lgrep_curbuf<cr>",          desc = "Buffer" },
            { "<leader>sc", "<cmd>FzfLua command_history<cr>",       desc = "Command History" },
            { "<leader>sC", "<cmd>FzfLua commands<cr>",              desc = "Commands" },
            { "<leader>sd", "<cmd>FzfLua diagnostics_document<cr>",  desc = "Document diagnostics" },
            { "<leader>sD", "<cmd>FzfLua diagnostics_workspace<cr>", desc = "Workspace diagnostics" },
            { "<leader>sh", "<cmd>FzfLua help_tags<cr>",             desc = "Help Pages" },
            { "<leader>sH", "<cmd>FzfLua highlights<cr>",            desc = "Search Highlight Groups" },
            { "<leader>sk", "<cmd>FzfLua keymaps<cr>",               desc = "Key Maps" },
            { "<leader>sM", "<cmd>FzfLua man_pages<cr>",             desc = "Man Pages" },
            { "<leader>sm", "<cmd>FzfLua marks<cr>",                 desc = "Jump to Mark" },
            { "<leader>so", "<cmd>FzfLua vim_options<cr>",           desc = "Options" },
            { "<leader>sR", "<cmd>FzfLua resume<cr>",                desc = "Resume" },
        },
    },
}
