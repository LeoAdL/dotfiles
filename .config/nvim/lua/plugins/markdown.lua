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
    {
        'MeanderingProgrammer/markdown.nvim',
        name = 'render-markdown', -- Only needed if you have another plugin named markdown.nvim
        dependencies = { 'nvim-treesitter/nvim-treesitter' },
        config = function()
            require('render-markdown').setup({})
        end,
    },

    {
        "tadmccorkle/markdown.nvim",
        ft = "markdown", -- or 'event = "VeryLazy"'
        opts = {
            -- configuration here or empty for defaults
        },
    }
}
