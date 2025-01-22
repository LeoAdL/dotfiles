return { {
    "stevearc/conform.nvim",
    lazy = true,
    config = function()
        require("conform").setup({
            formatters_by_ft = {
                tex = { "latexindent", },
                yaml = { "yamlfmt" },
                zsh = { "beautysh" },
                markdown = { "markdownlint-cli2" },
            },
            format_on_save = {
                -- These options will be passed to conform.format()
                lsp_format = "fallback",
                timeout_ms = 20000
            },
        })
    end,
} }
