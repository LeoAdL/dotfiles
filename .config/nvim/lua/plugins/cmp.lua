return {
    { "hrsh7th/cmp-path", enabled = false },
    {
        "hrsh7th/nvim-cmp",
        version = false, -- last release is way too old
        event = "InsertEnter",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
            "saadparwaiz1/cmp_luasnip",
            "kdheepak/cmp-latex-symbols",
            "hrsh7th/cmp-omni",
            "jc-doyle/cmp-pandoc-references",
            "hrsh7th/cmp-nvim-lsp-signature-help",
            "FelipeLema/cmp-async-path",
        },
        opts = function()
            vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
            local cmp = require("cmp")
            local defaults = require("cmp.config.default")()
            return {
                completion = {
                    completeopt = "menu,menuone,noinsert",
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
                    { name = "nvim_lsp" },
                    { name = "nvim_lsp_signature_help" },
                    { name = "luasnip", option = { show_autosnippets = true } },
                    { name = "buffer" },
                    { name = "async_path" },
                    { name = "pandoc_references" },
                    {
                        name = "latex_symbols",
                        option = {
                            strategy = 2, -- mixed
                        },
                    },
                    {
                        name = "omni",
                        option = {},
                    },
                }),
                formatting = {
                    format = function(_, item)
                        local icons = require("lazyvim.config").icons.kinds
                        if icons[item.kind] then
                            item.kind = icons[item.kind] .. item.kind
                        end
                        return item
                    end,
                },
                experimental = {
                    ghost_text = {
                        hl_group = "CmpGhostText",
                    },
                },
                sorting = defaults.sorting,
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
