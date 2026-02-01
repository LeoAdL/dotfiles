return {
    {
        "mason-org/mason.nvim",
        opts = {},
    },
    {
        "WhoIsSethDaniel/mason-tool-installer.nvim",
        cmd = "MasonToolsInstallSync",
        opts = {
            ensure_installed = {
                "lua-language-server",
                "beautysh",
                "selene",
                "marksman",
                "shfmt",
                "vale",
                "yamlfmt",
            },
        },
    },
    {
        "neovim/nvim-lspconfig",
        opts = {
            -- options for vim.diagnostic.config()
            diagnostics = {
                underline = false,
                update_in_insert = false,
                virtual_text = false,
                severity_sort = true,
                signs = {
                    text = {
                        [vim.diagnostic.severity.ERROR] = '',
                        [vim.diagnostic.severity.WARN] = '',
                        [vim.diagnostic.severity.INFO] = '',
                        [vim.diagnostic.severity.HINT] = '',
                    },
                },
            },
            inlay_hints = {
                enabled = true,
            },
            -- Enable this to enable the builtin LSP code lenses on Neovim >= 0.10.0
            -- Be aware that you also will need to properly configure your LSP server to
            -- provide the code lenses.
            codelens = {
                enabled = true,
            },
            capabilities = {
                workspace = {
                    fileOperations = {
                        didRename = true,
                        willRename = true,
                    },
                },
            },
            -- LSP Server Settings
            ---@type lspconfig.options
            ---
            servers = {
                texlab = {
                    settings = {
                        texlab = {
                            auxDirectory = ".",
                            bibtexFormatter = "texlab",
                            build = {
                                executable = "",
                                args = { "-X",
                                    "compile",
                                    "%f",
                                    "--synctex",
                                    "--keep-logs",
                                    "--keep-intermediates"
                                },
                                forwardSearchAfter = false,
                                onSave = false,
                            },
                            forwardSearch = {
                                executable = "sioyek",
                                args = {
                                    '--reuse-window',
                                    '--execute-command',
                                    'toggle_synctex', -- Open Sioyek in synctex mode.
                                    '--inverse-search',
                                    'nvr --remote %%1 -c %%2',
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
                nil_ls = {},
                ts_ls = {},
                harper_ls = {},
                basedpyright = {
                },
                -- ltex_plus = {
                --     filetypes = { 'bib', 'gitcommit', 'markdown', 'org', 'plaintex', 'rst', 'rnoweb', 'tex', 'pandoc', 'quarto', 'rmd', 'mail' },
                --
                --     settings = {
                --         ltex = {
                --             additionalRules = {
                --                 enablePickyRules = true,
                --             },
                --         },
                --     },
                -- },
                lua_ls = {
                    -- mason = false, -- set to false if you don't want this server to be installed with mason
                    settings = {
                        Lua = {
                            workspace = {
                                checkThirdParty = false,
                            },
                            diagnostics = { globals = { 'vim' } },
                            completion = {
                                callSnippet = "Replace",
                            },
                        },
                    },
                },
            },
        },
        config = function(_, opts)
            local servers = opts.servers
            for server, settings in pairs(servers) do
                vim.lsp.config(server, settings)
                vim.lsp.enable(server)
            end
            vim.diagnostic.config(opts.diagnostics)
        end
    }
}
