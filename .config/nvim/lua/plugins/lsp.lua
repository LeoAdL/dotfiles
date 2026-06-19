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
                "ruff",
                "basedpyright"
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
            servers = {
                texlab = {
                    settings = {
                        texlab = {
                            auxDirectory = ".",
                            bibtexFormatter = "texlab",
                            build = {
                                executable = "latexmk", -- (Added this placeholder because it was "" in your snippet!)
                                args = {
                                    "-X",
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
                                    'toggle_synctex',
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
                ts_ls = {},
                harper_ls = {},
                basedpyright = {},
                lua_ls = {
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
            -- Apply diagnostic configuration
            vim.diagnostic.config(opts.diagnostics)

            -- Modern Neovim 0.11+ native LSP setup
            local servers = opts.servers
            for server, settings in pairs(servers) do
                vim.lsp.config(server, settings)
                vim.lsp.enable(server)
            end

            -- Enable inlay hints globally
            if vim.lsp.inlay_hint then
                vim.lsp.inlay_hint.enable(true)
            end
        end
    }
}
