return {
  {
    "matbme/JABS.nvim",
    config = function()
      require("jabs").setup({})
    end,
    keys = {
      {
        "<leader>bb",
        function()
          require("jabs").open()
        end,
        desc = "Switch Buffer",
      },
    },
  },
}
