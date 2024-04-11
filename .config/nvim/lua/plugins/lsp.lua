return {
    {
        "williamboman/mason.nvim",
        config = true,
    },
    {
        "WhoIsSethDaniel/mason-tool-installer.nvim",
        opts = { ensure_installed = { "beautysh", "latexindent", "yamlfmt", "markdownlint", "markdownlint-cli2", "selene" } },
    },
    {
        "williamboman/mason-lspconfig.nvim",
        config = function()
            require("mason-lspconfig").setup {
            }
        end
    },
    {
        "jhofscheier/ltex-utils.nvim",
        dependencies = {
            "neovim/nvim-lspconfig",
            "nvim-telescope/telescope.nvim",
            -- "nvim-telescope/telescope-fzf-native.nvim", -- optional
        },
        opts = {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
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
            -- LSP Server Settings
            ---@type lspconfig.options
            servers = {
                texlab = {
                    settings = {
                        texlab = {
                            auxDirectory = ".",
                            bibtexFormatter = "texlab",
                            build = {
                                executable = "tectonic",
                                args = { "-X",
                                    "compile",
                                    "%f",
                                    "--synctex",
                                    "--keep-logs",
                                    "--keep-intermediates"
                                },
                                forwardSearchAfter = false,
                                onSave = true,
                            },
                            forwardSearch = {
                                executable = "sioyek",
                                args = {
                                    '--reuse-window',
                                    '--execute-command',
                                    'toggle_synctex', -- Open Sioyek in synctex mode.
                                    '--inverse-search',
                                    'nvr --remote-silent %%1 -c %%2',
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
                marksman = {},
                julials = {},
                pyright = {
                },
                ruff_lsp = {
                    on_attach = function(client, bufnr)
                        -- Disable hover in favor of Pyright
                        client.server_capabilities.hoverProvider = false
                    end
                },
                ltex = {
                    filetypes = { 'bib', 'gitcommit', 'markdown', 'org', 'plaintex', 'rst', 'rnoweb', 'tex', 'pandoc', 'quarto', 'rmd', 'mail' },

                    on_attach = function(client, bufnr)
                        -- rest of your on_attach process.
                        require("ltex-utils").on_attach(bufnr)
                    end,
                    settings = {
                        ltex = {
                            additionalRules = {
                                enablePickyRules = true,
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
            local mason_lspconfig = require 'mason-lspconfig'
            local servers = opts.servers
            mason_lspconfig.setup {
                ensure_installed = vim.tbl_keys(servers),
            }
            local capabilities = vim.lsp.protocol.make_client_capabilities()
            capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
            mason_lspconfig.setup_handlers {
                function(server_name)
                    require('lspconfig')[server_name].setup {
                        capabilities = capabilities,
                        on_attach = servers[server_name].on_attach,
                        settings = servers[server_name].settings,
                        filetypes = (servers[server_name] or {}).filetypes,
                    }
                end,
            }
            vim.diagnostic.config(opts.diagnostics)
            vim.fn.sign_define('DiagnosticSignError', { text = '', texthl = 'DiagnosticSignError' })
            vim.fn.sign_define('DiagnosticSignWarn', { text = '', texthl = 'DiagnosticSignWarn' })
            vim.fn.sign_define('DiagnosticSignInfo', { text = '', texthl = 'DiagnosticSignInfo' })
            vim.fn.sign_define('DiagnosticSignHint', { text = '', texthl = 'DiagnosticSignHint' })
        end
    }
}
