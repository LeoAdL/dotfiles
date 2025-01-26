return {
    {
        "williamboman/mason.nvim",
        cmd = "Mason",
        opts = {},
    },
    {
        "williamboman/mason-lspconfig.nvim",
        opts = {},
    },
    {
        "WhoIsSethDaniel/mason-tool-installer.nvim",
        lazy = false,
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
        event = { "BufReadPost", "BufWritePost", "BufNewFile" },
        dependencies = {
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
            "barreiroleo/ltex_extra.nvim",
        },
        opts = {
            -- options for vim.diagnostic.config()
            diagnostics = {
                underline = false,
                update_in_insert = false,
                virtual_text = false,
                severity_sort = true,
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
                basedpyright = {
                },
                ltex_plus = {
                    filetypes = { 'bib', 'gitcommit', 'markdown', 'org', 'plaintex', 'rst', 'rnoweb', 'tex', 'pandoc', 'quarto', 'rmd', 'mail' },

                    on_attach = function(client, bufnr)
                        -- rest of your on_attach process.
                        require("ltex_extra").setup {
                            -- table <string> : languages for witch dictionaries will be loaded, e.g. { "es-AR", "en-US" }
                            -- https://valentjn.github.io/ltex/supported-languages.html#natural-languages
                            load_langs = { "en-US" }, -- en-US as default
                            -- boolean : whether to load dictionaries on startup
                            init_check = true,        -- string : relative or absolute path to store dictionaries
                            -- e.g. subfolder in the project root or the current working directory: ".ltex"
                            -- e.g. shared files for all projects:  vim.fn.expand("~") .. "/.local/share/ltex"
                            path = "", -- project root or current working directory
                            -- string : "none", "trace", "debug", "info", "warn", "error", "fatal"
                            log_level = "none",
                            -- table : configurations of the ltex language server.
                            -- Only if you are calling the server from ltex_extra
                            server_opts = nil
                        }
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
            local capabilities = vim.lsp.protocol.make_client_capabilities()
            capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
            for server in pairs(opts.servers) do
                local server_opts = vim.tbl_deep_extend("force", {
                    capabilities = capabilities,
                }, servers[server])
                require("lspconfig")[server].setup(server_opts)
            end
            vim.diagnostic.config(opts.diagnostics)
            vim.fn.sign_define('DiagnosticSignError', { text = '', texthl = 'DiagnosticSignError' })
            vim.fn.sign_define('DiagnosticSignWarn', { text = '', texthl = 'DiagnosticSignWarn' })
            vim.fn.sign_define('DiagnosticSignInfo', { text = '', texthl = 'DiagnosticSignInfo' })
            vim.fn.sign_define('DiagnosticSignHint', { text = '', texthl = 'DiagnosticSignHint' })
        end
    }
}
