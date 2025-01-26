return {
    {
        "kevinhwang91/nvim-bqf",
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
        opts = { auto_resize_height = true, wrap = true },
        keys = {
            { '<space>q', vim.diagnostic.setloclist, desc = "LSP loclist" }
        }
    },
}
