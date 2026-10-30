return {
    {
        "lervag/vimtex",
        lazy = false,
        init = function()
            vim.g.vimtex_mappings_disable = { ["n"] = { "K" } } -- disable `K` as it conflicts with LSP hover
            vim.g.vimtex_quickfix_method = vim.fn.executable("pplatex") == 1 and "pplatex" or "latexlog"
            vim.g.vimtex_complete_enabled = 0
            vim.g.vimtex_quickfix_open_on_warning = 0
            vim.g.vimtex_view_method = "skim"
        end,
        keys = {
            { "<localLeader>l", "", desc = "+vimtex", ft = "tex" },
        },
    },
}
