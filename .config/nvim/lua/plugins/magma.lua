return {
  {
    "WhiteBlackGoose/magma-nvim-goose",
    build = ":UpdateRemotePlugins",
    config = function()
      vim.g.magma_automatically_open_output = false
      vim.g.magma_image_provider = "kitty"
    end,
    keys = {
      { "<LocalLeader>r", "<cmd>MagmaEvaluateVisual<cr>", mode = "v", desc = "Evaluate Operator" },
      { "<LocalLeader>r", "<CMD>MagmaEvaluateOperator<CR>", desc = "MagmaEvaluateOperator" },
      { "<LocalLeader>rr", "<CMD>MagmaEvaluateLine<CR>", desc = "MagmaEvaluateLine" },
      { "<LocalLeader>rc", "<CMD>MagmaReevaluateCell<CR>", desc = "MagmaReevaluateCell" },
      { "<LocalLeader>rd", "<CMD>MagmaDelete<CR>", desc = "MagmaDelete" },
      { "<LocalLeader>ro", "<CMD>MagmaShowOutput<CR>", desc = "MagmaShowOutput" },
    },
  },
}
