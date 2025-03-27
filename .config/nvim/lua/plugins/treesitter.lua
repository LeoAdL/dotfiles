return {
    {
        "nvim-treesitter/nvim-treesitter",
        version = false, -- last release is way too old and doesn't work on Windows
        event = { "BufReadPost", "BufWritePost", "BufNewFile" },
        build = ":TSUpdate",
        config = function()
            local configs = require("nvim-treesitter.configs")

            configs.setup({
                ensure_installed = {},
                sync_install = false,
                highlight = { enable = true },
                indent = { enable = true },
            })
        end
    },
    {
        "nvim-treesitter/nvim-treesitter-context",
        event = { "BufReadPost", "BufWritePost", "BufNewFile" },
        dependencies = { "nvim-treesitter/nvim-treesitter" },
        opts = {},
    },
}
