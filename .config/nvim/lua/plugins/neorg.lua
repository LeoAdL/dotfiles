return {
  {
    "nvim-neorg/neorg",
    build = ":Neorg sync-parsers",
    opts = {
      load = {
        ["core.defaults"] = {}, -- Loads default behaviour
        ["core.norg.concealer"] = {}, -- Adds pretty icons to your documents
        ["core.norg.qol.toc"] = {},
        ["core.norg.completion"] = { config = { engine = "nvim-cmp" } },
        ["core.integrations.telescope"] = {},
        ["core.norg.dirman"] = { -- Manages Neorg workspaces
          config = {
            workspaces = {
              notes = "~/org/notes",
            },
          },
        },
      },
    },
    dependencies = { { "nvim-lua/plenary.nvim" }, { "nvim-neorg/neorg-telescope" } },
  },
}
