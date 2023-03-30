return {
  {
    "jose-elias-alvarez/null-ls.nvim",
    opts = function()
      local null_ls = require("null-ls")
      return {
        proselint = {
          filetypes = { "markdown", "tex", "mail" },
        },
        sources = {
          null_ls.builtins.diagnostics.cspell,
          null_ls.builtins.code_actions.cspell,
          null_ls.builtins.diagnostics.ltrs,
          null_ls.builtins.code_actions.ltrs,
          null_ls.builtins.diagnostics.gitlint,
          null_ls.builtins.formatting.black,
          null_ls.builtins.formatting.isort,
          null_ls.builtins.diagnostics.ruff,
          null_ls.builtins.formatting.ruff,
          null_ls.builtins.diagnostics.chktex,
          null_ls.builtins.completion.luasnip,
          null_ls.builtins.diagnostics.luacheck,
          null_ls.builtins.code_actions.gitsigns,
          null_ls.builtins.formatting.prettierd,
          null_ls.builtins.formatting.stylua,
          null_ls.builtins.formatting.beautysh,
          null_ls.builtins.formatting.shfmt,
          null_ls.builtins.diagnostics.zsh,
          null_ls.builtins.diagnostics.yamllint,
          null_ls.builtins.formatting.yamlfmt,
        },
      }
    end,
  },
}
