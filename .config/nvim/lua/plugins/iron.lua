return {
    {
        "Vigemus/iron.nvim",
        config = function()
            local iron = require("iron.core")
            local view = require("iron.view")

            iron.setup({
                config = {
                    -- Whether a repl should be discarded or not
                    scratch_repl = false,
                    -- Your repl definitions come here
                    repl_definition = {
                        sh = {
                            -- Can be a table or a function that
                            -- returns a table (see below)
                            command = { "zsh" },
                        },
                    },
                    -- How the repl window will be displayed
                    -- See below for more information
                    repl_open_cmd = view.split.vertical.botright(0.4),
                },
                -- Iron doesn't set keymaps by default anymore.
                -- You can set them here or manually add keymaps to the functions in iron.core
                keymaps = {
                    send_motion = "<space>rc",
                    visual_send = "<space>rc",
                    send_file = "<space>rf",
                    send_line = "<space>rl",
                    send_mark = "<space>rm",
                    mark_motion = "<space>mc",
                    mark_visual = "<space>mc",
                    remove_mark = "<space>md",
                    cr = "<space>s<cr>",
                    interrupt = "<space>s<space>",
                    exit = "<space>sq",
                    clear = "<space>cl",
                },
                -- If the highlight is on, you can change how it looks
                -- For the available options, check nvim_set_hl
                highlight = {
                    italic = true,
                },
                ignore_blank_lines = true, -- ignore blank lines when sending visual select lines
            })

            -- iron also has a list of commands, see :h iron-commands for all available commands
            vim.keymap.set("n", "<space>Rs", "<cmd>IronRepl<cr>")
            vim.keymap.set("n", "<space>Rr", "<cmd>IronRestart<cr>")
            vim.keymap.set("n", "<space>Rf", "<cmd>IronFocus<cr>")
            vim.keymap.set("n", "<space>Rh", "<cmd>IronHide<cr>")
        end,
    },
}
