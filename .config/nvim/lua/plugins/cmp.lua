return {
    {
        "hrsh7th/nvim-cmp",
        version = false, -- last release is way too old
        event = "InsertEnter",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "ray-x/cmp-treesitter",
            "onsails/lspkind.nvim",
            "hrsh7th/cmp-buffer",
            "saadparwaiz1/cmp_luasnip",
            "kdheepak/cmp-latex-symbols",
            "hrsh7th/cmp-omni",
            "jc-doyle/cmp-pandoc-references",
            "hrsh7th/cmp-nvim-lsp-signature-help",
            "FelipeLema/cmp-async-path",
        },
        config = function()
            local cmp = require("cmp")
            require 'cmp'.setup {
                completion = {
                    completeopt = "menu,menuone,preview",
                },
                snippet = {
                    expand = function(args)
                        require("luasnip").lsp_expand(args.body)
                    end,
                },
                mapping = cmp.mapping.preset.insert({
                    ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
                    ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
                    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-e>"] = cmp.mapping.abort(),
                    ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                    ["<S-CR>"] = cmp.mapping.confirm({
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = true,
                    }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                }),
                sources = cmp.config.sources({
                    { name = "nvim_lsp",                priority = 10000 },
                    { name = "nvim_lsp_signature_help", priority = 5000 },
                    { name = "luasnip",                 option = { show_autosnippets = true }, priority = 3000 },
                    { name = "treesitter",              option = { show_autosnippets = true }, priority = 1000 },
                    { name = "buffer",                  priority = 200 },
                    { name = "async_path",              priority = 200 },
                    { name = "pandoc_references",       priority = 800 },
                }),
                formatting = {
                    format = require('lspkind').cmp_format({
                        mode = 'symbol',       -- show only symbol annotations
                        maxwidth = 50,         -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
                        ellipsis_char = '...', -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
                    })
                },
            }
        end,
    },
    {
        "aspeddro/cmp-pandoc.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
        },
    },
}
