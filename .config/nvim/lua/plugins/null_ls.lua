return {
  {
    "jose-elias-alvarez/null-ls.nvim",
    opts = function()
      local nls = require("null-ls")
      return {
        sources = {
          nls.builtins.formatting.black,
          nls.builtins.formatting.latexindent,
          nls.builtins.formatting.isort,
          nls.builtins.code_actions.proselint,
          nls.builtins.formatting.ruff,
          nls.builtins.diagnostics.chktex,
          nls.builtins.completion.luasnip,
          nls.builtins.diagnostics.luacheck,
          nls.builtins.formatting.stylua,
          nls.builtins.code_actions.gitsigns,
          nls.builtins.formatting.prettierd,
          nls.builtins.formatting.stylua,
        },
      }
    end,
  },
}
