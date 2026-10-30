return {
    {
        -- The `main` branch no longer takes highlight/indent/ensure_installed opts:
        -- parsers are installed with install() and highlighting is started per buffer.
        "nvim-treesitter/nvim-treesitter",
        branch = "main",
        lazy = false, -- does not support lazy-loading
        build = ":TSUpdate",
        config = function()
            require("nvim-treesitter").install({
                "bash",
                "c",
                "diff",
                "html",
                "javascript",
                "jsdoc",
                "json",
                "lua",
                "luadoc",
                "luap",
                "markdown",
                "markdown_inline",
                "printf",
                "python",
                "query",
                "regex",
                "toml",
                "tsx",
                "typescript",
                "vim",
                "vimdoc",
                "xml",
                "yaml",
            })
            vim.api.nvim_create_autocmd("FileType", {
                group = vim.api.nvim_create_augroup("custom_treesitter", { clear = true }),
                callback = function(ev)
                    -- vimtex relies on regex syntax for LaTeX
                    if ev.match == "tex" then
                        return
                    end
                    if pcall(vim.treesitter.start, ev.buf) then
                        vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
                    end
                end,
            })
        end,
    },
    {
        "nvim-treesitter/nvim-treesitter-context",
        event = "VeryLazy",
        opts = {},
    },
}
