return {
  {
    "WhiteBlackGoose/magma-nvim-goose",
    enabled = false,
    build = ":UpdateRemotePlugins",
    config = function()
      vim.g.magma_automatically_open_output = false
      vim.g.magma_image_provider = "kitty"
    end,
    keys = {
      { "<LocalLeader>R",  "<cmd>MagmaEvaluateVisual<cr>",   mode = "v",                    desc = "Evaluate Operator" },
      { "<LocalLeader>R",  "<CMD>MagmaEvaluateOperator<CR>", desc = "MagmaEvaluateOperator" },
      { "<LocalLeader>Rr", "<CMD>MagmaEvaluateLine<CR>",     desc = "MagmaEvaluateLine" },
      { "<LocalLeader>Rc", "<CMD>MagmaReevaluateCell<CR>",   desc = "MagmaReevaluateCell" },
      { "<LocalLeader>Rd", "<CMD>MagmaDelete<CR>",           desc = "MagmaDelete" },
      { "<LocalLeader>Ro", "<CMD>MagmaShowOutput<CR>",       desc = "MagmaShowOutput" },
    },
  },
}
