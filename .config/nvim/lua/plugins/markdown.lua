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
    { "ellisonleao/glow.nvim", config = true, cmd = "Glow" },
    {
        "jghauser/follow-md-links.nvim",
    },
    {
        "lukas-reineke/headlines.nvim",
        dependencies = "nvim-treesitter/nvim-treesitter",
        config = true, -- or `opts = {}`
    },
}
