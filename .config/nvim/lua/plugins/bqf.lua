return { {
    'junegunn/fzf',
    run = function()
        vim.fn['fzf#install']()
    end
},
    {
        "kevinhwang91/nvim-bqf",
        ft = "qf",
        config = function()
            require('bqf').setup({ func_map = { fzffilter = "<C-f>", pscrolldown = "zf" } })
        end,
        opts = { auto_resize_height = true, wrap = true }
    },
}