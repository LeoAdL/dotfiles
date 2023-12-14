return {
    "NeogitOrg/neogit",
    enabled = false,
    dependencies = "nvim-lua/plenary.nvim",
    config = function()
        local neogit = require('neogit')

        neogit.setup {}
    end
}
