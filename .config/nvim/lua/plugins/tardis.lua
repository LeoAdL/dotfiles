return {
    'fredeeb/tardis.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = true,
    keys = {
        { "<leader>gt", "<cmd>Tardis<cr>", desc = "Timemachine" }
    },
}
