return {
    -- add symbols-outline
    {
        "simrat39/symbols-outline.nvim",
        cmd = "SymbolsOutline",
        keys = { { "<leader>cs", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" } },
        opts = {
            -- add your options that should be passed to the setup() function here
            position = "right",
        },
    },

    {
        "neovim/nvim-lspconfig",
        opts = {
            -- options for vim.diagnostic.config()
            diagnostics = {
                underline = true,
                update_in_insert = false,
                virtual_text = { spacing = 4, prefix = "●" },
                severity_sort = true,
            },
            -- Automatically format on save
            -- options for vim.lsp.buf.format
            -- `bufnr` and `filter` is handled by the LazyVim formatter,
            -- but can be also overridden when specified
            format = {
                formatting_options = nil,
                timeout_ms = nil,
            },
            -- LSP Server Settings
            ---@type lspconfig.options
            servers = {
                efm = { init_options = { documentFormatting = true } },
                marksman = {},
                texlab = {
                    keys = {
                        { "<Leader>K", "<plug>(vimtex-doc-package)", desc = "Vimtex Docs", silent = true },
                    },
                    settings = {
                        texlab = {
                            auxDirectory = ".",
                            bibtexFormatter = "texlab",
                            build = {
                                executable = "latexmk",
                                args = { "-xelatex", "-synctex=1" },
                                forwardSearchAfter = false,
                                onSave = true,
                            },
                            forwardSearch = {
                                executable = "sioyek",
                                args = {
                                    '--reuse-window',
                                    '--execute-command', 'toggle_synctex', -- Open Sioyek in synctex mode.
                                    '--inverse-search',
                                    'nvim --headless -c "VimtexInverseSearch %%2 %%1"',
                                    '--forward-search-file', '%f',
                                    '--forward-search-line', '%l', '%p'
                                },
                            },
                            chktex = {
                                onEdit = false,
                                onOpenAndSave = false,
                            },
                            diagnosticsDelay = 300,
                            formatterLineLength = 80,
                            latexFormatter = "none",
                        },
                    },
                },
                julials = {},
                pyright = {},
                ruff_lsp = {},
                ltex = {
                    settings = {
                        ltex = {
                            additionalRules = {
                                enablePickyRules = true,
                                motherTongue = "fr",
                                dictionary = {
                                    ['en-US'] = words,
                                },
                            },
                        },
                    },
                },
                lua_ls = {
                    -- mason = false, -- set to false if you don't want this server to be installed with mason
                    settings = {
                        Lua = {
                            workspace = {
                                checkThirdParty = false,
                            },
                            completion = {
                                callSnippet = "Replace",
                            },
                        },
                    },
                },
            },
        },
        config = function(_, opts)
            local path = vim.fn.stdpath 'config' .. '/spell/en.utf-8.add'
            local words = {}

            for word in io.open(path, 'r'):lines() do
                table.insert(words, word)
            end
            local servers = opts.servers
            local capabilities = require('cmp_nvim_lsp').default_capabilities()
            local lspconfig = require('lspconfig')
            for server, server_opts in pairs(servers) do
                lspconfig[server].setup { server_opts, capabilities = capabilities }
            end
        end
    }
    ,
    {
        "barreiroleo/ltex_extra.nvim",
        ft = { "markdown", "tex" },
        dependencies = { "neovim/nvim-lspconfig" },
        -- yes, you can use the opts field, just I'm showing the setup explicitly
        config = function()
            require("ltex_extra").setup({
                server_opts = {},
            })
        end,
    },
}
