return {
    {
        "jose-elias-alvarez/null-ls.nvim",
        opts = function()
            local lsp_formatting = function(bufnr)
                vim.lsp.buf.format({
                    filter = function(client)
                        -- apply whatever logic you want (in this example, we'll only use null-ls)
                        return client.name == "null-ls"
                    end,
                    bufnr = bufnr,
                })
            end

            -- if you want to set up formatting on save, you can use this as a callback
            local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

            -- add to your shared on_attach callback
            local on_attach = function(client, bufnr)
                if client.supports_method("textDocument/formatting") then
                    vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
                    vim.api.nvim_create_autocmd("BufWritePre", {
                        group = augroup,
                        buffer = bufnr,
                        callback = function()
                            lsp_formatting(bufnr)
                        end,
                    })
                end
            end
            local null_ls = require("null-ls")
            return {
                sources = {
                    null_ls.builtins.diagnostics.ltrs,
                    null_ls.builtins.code_actions.ltrs,
                    null_ls.builtins.diagnostics.gitlint,
                    null_ls.builtins.formatting.black,
                    null_ls.builtins.diagnostics.ruff,
                    null_ls.builtins.formatting.ruff,
                    null_ls.builtins.diagnostics.chktex,
                    null_ls.builtins.completion.luasnip,
                    null_ls.builtins.diagnostics.luacheck,
                    null_ls.builtins.code_actions.gitsigns,
                    null_ls.builtins.formatting.prettierd,
                    null_ls.builtins.formatting.stylua,
                    null_ls.builtins.formatting.beautysh,
                    null_ls.builtins.diagnostics.zsh,
                    null_ls.builtins.code_actions.shellcheck,
                    null_ls.builtins.diagnostics.shellcheck,
                    null_ls.builtins.diagnostics.yamllint,
                    null_ls.builtins.formatting.yamlfmt,
                    null_ls.builtins.formatting.cbfmt,
                    null_ls.builtins.formatting.markdown_toc,
                },
            }
        end,
    },
}
