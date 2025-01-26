return {
    {
        "kevinhwang91/nvim-bqf",
        event = { "BufReadPost", "BufWritePost", "BufNewFile" },
        dependencies = {
            'junegunn/fzf',
        },
        ft = "qf",
        build = function()
            if vim.loop.os_uname().sysname ~= 'Windows_NT'
            then
                vim.fn['fzf#install']()
            end
        end,
        config = function()
            require('bqf').setup({ func_map = { fzffilter = "<C-f>", pscrolldown = "zf" } })
        end,
        opts = { auto_resize_height = true, wrap = true }
    },
}
