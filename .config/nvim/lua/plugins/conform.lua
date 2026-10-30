return { {
    "stevearc/conform.nvim",
    cmd = "ConformInfo",
    event = "BufWritePre",
    opts = {
        formatters_by_ft = {
            lua = { "stylua" },
            python = { "ruff_organize_imports", "ruff_format" },
            sh = { "shfmt" },
            zsh = { "beautysh" },
            tex = { "latexindent" },
            yaml = { "yamlfmt" },
            markdown = { "markdownlint-cli2" },
        },
        format_on_save = {
            lsp_format = "fallback",
            timeout_ms = 3000, -- formatting on save blocks the editor until it finishes
        },
    },
} }
