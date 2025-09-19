return {
    {
        "saghen/blink.cmp",
        version = "*",
        dependencies = {
            "rafamadriz/friendly-snippets",
            { 'L3MON4D3/LuaSnip', version = 'v2.*' },
            -- add blink.compat to dependencies
        },
        event = "InsertEnter",

        ---@module 'blink.cmp'
        ---@type blink.cmp.Config
        opts = {
            appearance = {
                -- sets the fallback highlight groups to nvim-cmp's highlight groups
                -- useful for when your theme doesn't support blink.cmp
                -- will be removed in a future release, assuming themes add support
                use_nvim_cmp_as_default = false,
                -- set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
                -- adjusts spacing to ensure icons are aligned
                nerd_font_variant = "mono",
            },
            keymap = {
                preset = "super-tab",
            },
            snippets = { preset = 'luasnip' },
            completion = {
                accept = {
                    -- experimental auto-brackets support
                    auto_brackets = {
                        enabled = true,
                    },
                },
                list = { selection = { preselect = false, auto_insert = true } },
                menu = {
                    draw = {
                        treesitter = { "lsp" },
                    },
                },
                documentation = {
                    auto_show = true,
                    auto_show_delay_ms = 200,
                },
            },

            -- experimental signature help support
            signature = { enabled = true },

            sources = {
                -- adding any nvim-cmp sources here will enable them
                -- with blink.compat
                default = { "lsp", "path", "snippets", "buffer", "cmdline", "omni" },
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
                    org = { 'orgmode', "lsp", "path", "snippets", "buffer", "cmdline", "omni" },
                    markdown = { 'markdown', "lsp", "path", "snippets", "buffer", "cmdline", "omni" },
                },
            },
        },
        opts_extend = { "sources.default" },
    },
}
