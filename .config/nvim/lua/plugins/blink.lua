return {
    {
        "saghen/blink.cmp",
        version = "*",
        dependencies = {
            { 'L3MON4D3/LuaSnip', version = 'v2.*' },
        },
        event = { "InsertEnter", "CmdlineEnter" },

        ---@module 'blink.cmp'
        ---@type blink.cmp.Config
        opts = {
            appearance = {
                use_nvim_cmp_as_default = false,
                nerd_font_variant = "mono",
            },
            keymap = {
                preset = "super-tab",
            },
            snippets = { preset = 'luasnip' },
            completion = {
                accept = {
                    auto_brackets = { enabled = true },
                },
                list = { selection = { preselect = false, auto_insert = true } },
                menu = {
                    draw = { treesitter = { "lsp" } },
                },
                documentation = {
                    auto_show = true,
                    auto_show_delay_ms = 200,
                },
            },

            signature = { enabled = true },

            sources = {
                default = { "lsp", "path", "snippets", "buffer", "omni" },
                providers = {
                    markdown = {
                        name = 'RenderMarkdown',
                        module = 'render-markdown.integ.blink',
                        fallbacks = { 'lsp' },
                    },
                    orgmode = {
                        name = 'Orgmode',
                        module = 'orgmode.org.autocompletion.blink',
                        fallbacks = { 'buffer' },
                    },
                },
                per_filetype = {
                    org = { 'orgmode', "lsp", "path", "snippets", "buffer", "omni" },
                    markdown = { 'markdown', "lsp", "path", "snippets", "buffer", "omni" },
                },
            },

            -- Terminal configuration
            term = {
                enabled = true,
                keymap = { preset = 'inherit' },
                sources = {},
            },

            -- Command-line configuration
            cmdline = {
                enabled = true,
                keymap = { preset = 'cmdline' },
                sources = { 'buffer', 'cmdline' },
                completion = {
                    list = {
                        selection = {
                            preselect = true,
                            auto_insert = true,
                        },
                    },
                    -- Default is false for cmdline, true for cmdwin (command-line window)
                    menu = {
                        auto_show = function(ctx) return ctx.mode == 'cmdwin' end
                    },
                    ghost_text = { enabled = true },
                }
            },
        },
        -- opts_extend MUST be a sibling of opts, not inside it!
        opts_extend = { "sources.default" },
    },
}
