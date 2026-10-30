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
        -- Run from the directory holding selene.toml (it isn't searched upward),
        -- otherwise selene flags every `vim` global when nvim starts elsewhere
        local function try_lint()
            lint.try_lint(nil, { cwd = vim.fs.root(0, "selene.toml") })
        end
        vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost", "InsertLeave" }, {
            group = vim.api.nvim_create_augroup("Linting", { clear = true }),
            callback = try_lint,
        })
        -- Lint the buffer that triggered loading once its filetype is set (the
        -- first BufReadPost runs before filetype detection)
        vim.schedule(try_lint)
    end
} }
