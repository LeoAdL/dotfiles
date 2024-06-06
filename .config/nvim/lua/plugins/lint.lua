return { {
    "mfussenegger/nvim-lint",
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
