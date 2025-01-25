return { {
    "mfussenegger/nvim-lint",
    events = { "BufWritePost", "BufReadPost", "InsertLeave" },
    config = function()
        require('lint').linters_by_ft = {
            lua = { 'selene', },
            -- tex = { 'chktex' },
            git = { 'gitlint' },
            yaml = { 'yamllint' },
            markdown = { 'markdownlint' },
        }
    end
} }
