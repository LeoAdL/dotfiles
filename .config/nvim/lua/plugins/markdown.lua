return {
    {
        "iamcco/markdown-preview.nvim",
        build = function()
            vim.fn["mkdp#util#install"]()
        end,
    },
    {
        "AckslD/nvim-FeMaco.lua",
        config = 'require("femaco").setup()',
    },
    { "ellisonleao/glow.nvim", config = true, cmd = "Glow", enabled = false },
    {
        "jghauser/follow-md-links.nvim",
    },
    {
        "bvolkmer/nvim-markdown-preview",
        enabled = false,
        config = function()
            vim.g.nvim_markdown_extraargs = { "--bibliography mybib.bib" }
        end,
    },
}
