return {
    {
        "mrjones2014/smart-splits.nvim",
        config = function()
            require("smart-splits").setup({ multiplexer_integration = "tmux" })
        end,
    },
}
