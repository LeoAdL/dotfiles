return {
    {
        "AckslD/nvim-FeMaco.lua",
        config = 'require("femaco").setup()',
    },
    { "ellisonleao/glow.nvim", config = true, cmd = "Glow" },
    {
        "jghauser/follow-md-links.nvim",
    },
    {
        "bvolkmer/nvim-markdown-preview",
        config = function()
            vim.g.nvim_markdown_extraargs = { "--bibliography mybib.bib" }
        end,
    },
}
