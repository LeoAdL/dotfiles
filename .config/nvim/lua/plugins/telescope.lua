local dependencies = { { "nvim-lua/plenary.nvim" }, { "debugloop/telescope-undo.nvim" } }
if vim.loop.os_uname().sysname ~= 'Windows_NT'
then
    table.insert(dependencies,
        {
            "nvim-telescope/telescope-fzf-native.nvim",
            build = "make",
            config = function()
                require("telescope").load_extension("fzf")
            end,
        })
end
return {
    { "stevearc/dressing.nvim" },
    {
        "telescope.nvim",
        dependencies = dependencies,
        config = function()
            local ts = require("telescope")
            local tsu = require("telescope-undo.actions")
            ts.setup({
                extensions = {
                    git_diffs = {
                        git_command = { "git", "log", "--oneline", "--decorate", "--all", "." } -- list result
                    },
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
                desc = "Undo history"
            },
            { "<leader>tt", "<cmd>Telescope<cr>",                       desc = "Open Telescope" },
            {
                "<leader>,",
                "<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>",
                desc = "Switch BSffer",
            },
            { "<leader>:",  "<cmd>Telescope command_history<cr>",       desc = "Command History" },
            { "<leader>fb", "<cmd>Telescope buffers<cr>",               desc = "Buffers" },
            { "<leader>ff", "<cmd>Telescope find_files<cr>",            desc = "Find Files (root dir)" },
            { "<leader>fr", "<cmd>Telescope oldfiles<cr>",              desc = "Recent" },
            -- git
            { "<leader>gc", "<cmd>Telescope git_commits<CR>",           desc = "commits" },
            { "<leader>/",  "<cmd>Telescope live_grep<CR>",             desc = "live grep" },
            { "<leader>gs", "<cmd>Telescope git_status<CR>",            desc = "status" },
            -- search
            { '<leader>s"', "<cmd>Telescope registers<cr>",             desc = "Registers" },
            { "<leader>sa", "<cmd>Telescope autocommands<cr>",          desc = "Auto Commands" },
            { "<leader>sb", "<cmd>Telescope lgrep_curbuf<cr>",          desc = "Buffer" },
            { "<leader>sc", "<cmd>Telescope command_history<cr>",       desc = "Command History" },
            { "<leader>sC", "<cmd>Telescope commands<cr>",              desc = "Commands" },
            { "<leader>sd", "<cmd>Telescope diagnostics_document<cr>",  desc = "Document diagnostics" },
            { "<leader>sD", "<cmd>Telescope diagnostics_workspace<cr>", desc = "Workspace diagnostics" },
            { "<leader>sS", "<cmd>Telescope lsp_document_symbols<cr>",  desc = "Workspace diagnostics" },
            { "<leader>sh", "<cmd>Telescope help_tags<cr>",             desc = "Help Pages" },
            { "<leader>sH", "<cmd>Telescope highlights<cr>",            desc = "Search Highlight Groups" },
            { "<leader>sk", "<cmd>Telescope keymaps<cr>",               desc = "Key Maps" },
            { "<leader>sM", "<cmd>Telescope man_pages<cr>",             desc = "Man Pages" },
            { "<leader>sm", "<cmd>Telescope marks<cr>",                 desc = "Jump to Mark" },
            { "<leader>so", "<cmd>Telescope vim_options<cr>",           desc = "Options" },
            { "<leader>sR", "<cmd>Telescope resume<cr>",                desc = "Resume" },
        },
    },
    {
        "adoyle-h/lsp-toggle.nvim",
        config = function()
            require("lsp-toggle").setup()
        end,
    },
}
