return {
  {
    "TimUntersberger/neogit",
    config = function()
      require("neogit").setup()
    end,
    keys = {
      {
        "<leader>gg",
        function()
          local neogit = require("neogit")

          -- open using defaults
          neogit.open()
        end,
        "Neogit",
      },
    },
  },
}
