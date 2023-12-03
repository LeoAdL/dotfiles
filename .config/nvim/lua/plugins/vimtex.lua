return {
    {
        "lervag/vimtex",
        enabled = true,
        config = function()
            vim.g.vimtex_complete_enabled = 0
            vim.g.vimtex_compiler_enabled = 0
            vim.g.vimtex_compiler_method = 'tectonic'
            vim.g.vimtex_quickfix_open_on_warning = 0
            vim.g.conceallevel = 2
            vim.g.vimtex_view_enabled = 0
            vim.g.vimtex_view_method = "sioyek"
            vim.g.vimtex_quickfix_enabled = 0
        end,
    },
}
