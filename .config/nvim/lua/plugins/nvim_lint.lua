return {
  {
    "mfussenegger/nvim-lint",
    config = function()
      require("lint").linters_by_ft = {
        lua = { "selene" },
        luau = { "selene" },
        yaml = { "yamllint" },
        tex = { "chktex", "vale" },
        markdown = { "vale" }
      }
      vim.api.nvim_create_autocmd({ "BufRead" }, {
        callback = function()
          require("lint").try_lint()
        end,
      })
    end,
  },
}
