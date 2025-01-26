return {
    {
        "iguanacucumber/magazine.nvim",
        name = "nvim-cmp", -- Otherwise highlighting gets messed up
        event = "InsertEnter",
        dependencies = {
            { "iguanacucumber/mag-nvim-lsp", name = "cmp-nvim-lsp", opts = {} },
            { "iguanacucumber/mag-buffer",   name = "cmp-buffer" },
            { "iguanacucumber/mag-cmdline",  name = "cmp-cmdline" },
            "ray-x/cmp-treesitter",
            "onsails/lspkind.nvim",
            "saadparwaiz1/cmp_luasnip",
            "hrsh7th/cmp-omni",
            "hrsh7th/cmp-nvim-lsp-signature-help",
            "https://codeberg.org/FelipeLema/cmp-async-path",
        },
        config = function()
            local cmp = require("cmp")
            local lspkind = require('lspkind')
            local has_words_before = function()
                unpack = unpack or table.unpack
                local line, col = unpack(vim.api.nvim_win_get_cursor(0))
                return col ~= 0 and
                    vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
            end
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
                    ["<S-CR>"] = cmp.mapping.confirm({
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = true,
                    }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                    ["<Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            -- You could replace select_next_item() with confirm({ select = true }) to get VS Code autocompletion behavior
                            cmp.select_next_item()
                        elseif vim.snippet.active({ direction = 1 }) then
                            vim.schedule(function()
                                vim.snippet.jump(1)
                            end)
                        elseif has_words_before() then
                            cmp.complete()
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                    ["<S-Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.select_prev_item()
                        elseif vim.snippet.active({ direction = -1 }) then
                            vim.schedule(function()
                                vim.snippet.jump(-1)
                            end)
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                }),
                sources = cmp.config.sources({
                    { name = "nvim_lsp",               priority = 10000 },
                    { name = "nvim-lsp-signature-help" },
                    { name = "omni",                   priority = 100 },
                    { name = "luasnip",                option = { show_autosnippets = true }, priority = 3000 },
                    { name = "treesitter",             option = { show_autosnippets = true }, priority = 1000 },
                    { name = "buffer",                 priority = 200 },
                    { name = "async_path",             priority = 200 },
                }),
                formatting = {
                    format = lspkind.cmp_format({
                        mode = 'symbol', -- show only symbol annotations
                        maxwidth = {
                            -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
                            -- can also be a function to dynamically calculate max width such as
                            -- menu = function() return math.floor(0.45 * vim.o.columns) end,
                            menu = 50,            -- leading text (labelDetails)
                            abbr = 50,            -- actual suggestion item
                        },
                        ellipsis_char = '...',    -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
                        show_labelDetails = true, -- show labelDetails in menu. Disabled by default
                    })
                },
            }
        end,
    },
}
