return {
    {
        "rcarriga/nvim-notify",
        enabled = true,
        config = function()
            vim.notify = require("notify")
        end
    }, }
