return {
    -- add gruvbox
    {
        "shaunsingh/nord.nvim",
        config = function()
            vim.g.nord_contrast = true
            vim.g.nord_uniform_diff_background = true
            require("nord").set()
        end,
    },
    { "nvim-lualine/lualine.nvim", opts = { theme = "nord" } },
    -- Configure LazyVim to load gruvbox
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "nord",
        },
    },
}
