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
            keymap = {
                preset = "super-tab",
            },
            snippets = { preset = 'luasnip' },
            completion = {
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
                default = { "lsp", "path", "snippets", "buffer" },
                providers = {
                    lazydev = {
                        name = "LazyDev",
                        module = "lazydev.integrations.blink",
                        score_offset = 100, -- show at a higher priority than lsp
                    },
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
                    lua = { inherit_defaults = true, 'lazydev' },
                    org = { inherit_defaults = true, 'orgmode' },
                    markdown = { inherit_defaults = true, 'markdown' },
                },
            },
        },
    },
}
