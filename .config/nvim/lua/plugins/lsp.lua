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
                "marksman",
                "selene",
                "shfmt",
                "stylua",
                "yamlfmt",
            },
        },
    },
    {
        -- Completion/types for the Neovim Lua API when editing this config
        "folke/lazydev.nvim",
        ft = "lua",
        opts = {
            library = {
                { path = "${3rd}/luv/library", words = { "vim%.uv" } },
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
                        [vim.diagnostic.severity.ERROR] = '',
                        [vim.diagnostic.severity.WARN] = '',
                        [vim.diagnostic.severity.INFO] = '',
                        [vim.diagnostic.severity.HINT] = '',
                    },
                },
            },
            servers = {
                -- vimtex handles compiling and forward/inverse search with sioyek
                texlab = {
                    settings = {
                        texlab = {
                            chktex = {
                                onEdit = false,
                                onOpenAndSave = false,
                            },
                            diagnosticsDelay = 300,
                            latexFormatter = "none",
                        },
                    },
                },
                marksman = {},
                harper_ls = {},
                basedpyright = {},
                ruff = {},
                lua_ls = {
                    settings = {
                        Lua = {
                            completion = {
                                callSnippet = "Replace",
                            },
                        },
                    },
                },
            },
        },
        config = function(_, opts)
            vim.diagnostic.config(opts.diagnostics)

            for server, settings in pairs(opts.servers) do
                vim.lsp.config(server, settings)
                vim.lsp.enable(server)
            end

            vim.lsp.inlay_hint.enable(true)
        end
    }
}
