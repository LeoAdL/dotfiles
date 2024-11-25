return { {
    'junegunn/fzf',
    build = function()
        if vim.loop.os_uname().sysname ~= 'Windows_NT'
        then
            vim.fn['fzf#install']()
        end
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
