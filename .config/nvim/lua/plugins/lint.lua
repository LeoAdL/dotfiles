return { {
    "mfussenegger/nvim-lint",
    event = { "BufReadPost", "BufNewFile" },
    config = function()
        local lint = require('lint')
        lint.linters_by_ft = {
            lua = { 'selene' },
            gitcommit = { 'gitlint' },
            yaml = { 'yamllint' },
            markdown = { 'markdownlint-cli2' },
        }
        vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost", "InsertLeave" }, {
            group = vim.api.nvim_create_augroup("Linting", { clear = true }),
            callback = function()
                lint.try_lint()
            end,
        })
        lint.try_lint() -- the buffer that triggered loading
    end
} }
