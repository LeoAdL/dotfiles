return {
    {
        "AckslD/nvim-FeMaco.lua",
        ft = "markdown",
        opts = {}
    },
    {
        "jghauser/follow-md-links.nvim",
        ft = "markdown",
        opts = {}
    },

    {
        'MeanderingProgrammer/render-markdown.nvim',
        ft = "markdown",
        -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
        -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
        dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
        ---@module 'render-markdown'
        ---@type render.md.UserConfig
        opts = {},
    },
    {
        "tadmccorkle/markdown.nvim",
        ft = "markdown", -- or 'event = "VeryLazy"'
        opts = {
            -- configuration here or empty for defaults
        },
    }
}
