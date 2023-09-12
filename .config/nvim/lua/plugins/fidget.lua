return {
  {
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup({
        sources = {                     -- Sources to configure
          ltex = {                      -- Name of source
            ignore = false,             -- Ignore notifications from this source
          },
        },
      })
    end,
    tag = "legacy",
  },
}
