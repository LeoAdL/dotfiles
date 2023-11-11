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
    { "barreiroleo/ltex_extra.nvim" },
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
                                onSave = false,
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
                    on_attach = function(client, bufnr)
                        -- rest of your on_attach process.
                        require("ltex_extra").setup {
                            -- table <string> : languages for witch dictionaries will be loaded, e.g. { "es-AR", "en-US" }
                            -- https://valentjn.github.io/ltex/supported-languages.html#natural-languages
                            load_langs = { "en-US" }, -- en-US as default
                            -- boolean : whether to load dictionaries on startup
                            init_check = true,
                            -- string : relative or absolute path to store dictionaries
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
                                motherTongue = "fr",
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
            local servers = opts.servers
            local capabilities = { require('cmp_nvim_lsp').default_capabilities() }
            for server in pairs(servers) do
                local server_opts = vim.tbl_deep_extend("force", {
                    capabilities = capabilities,
                }, servers[server])
                require("lspconfig")[server].setup(server_opts)
            end
            vim.diagnostic.config(opts.diagnostics)
        end
    }
    ,
}
