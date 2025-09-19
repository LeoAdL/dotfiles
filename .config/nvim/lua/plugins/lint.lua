return { {
    "mfussenegger/nvim-lint",
    events = { "BufWritePost", "BufReadPost", "InsertLeave" },
    config = function()
        local lint = require('lint')
        lint.linters_by_ft = {
            lua = { 'selene', },
            -- tex = { 'chktex' },
            git = { 'gitlint' },
            yaml = { 'yamllint' },
            markdown = { 'markdownlint' },
        }
        local lint_augroup = vim.api.nvim_create_augroup("Linting", { clear = true })

        vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
            group = lint_augroup,
            callback = function()
                -- Check if nvim-lint is available before trying to lint
                lint.try_lint()
            end,
        })
    end
} }
