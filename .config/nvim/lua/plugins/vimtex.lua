return {
    {
        "lervag/vimtex",
        ft = "tex",
        lazy = false,
        enabled = true,
        config = function()
            vim.g.vimtex_mappings_disable = { ["n"] = { "K" } } -- disable `K` as it conflicts with LSP hover
            vim.g.vimtex_quickfix_method = vim.fn.executable("pplatex") == 1 and "pplatex" or "latexlog"
            vim.g.vimtex_complete_enabled = 0
            vim.g.vimtex_syntax_enabled = 1
            vim.g.vimtex_quickfix_open_on_warning = 0
            vim.g.conceallevel = 2
            vim.g.vimtex_view_enabled = 1
            vim.g.vimtex_view_method = "sioyek"
            vim.g.vimtex_callback_progpath = '/opt/homebrew/bin/nvim'
        end,
        keys = {
            { "<localLeader>l", "", desc = "+vimtex", ft = "tex" },
        },
    },
}
