return {
  {
    "lervag/vimtex",
    config = function()
      vim.g.vimtex_complete_enabled = 0
      vim.g.vimtex_compiler_enabled = 0
      vim.g.maplocalleader = " "
      vim.g.vimtex_quickfix_open_on_warning = 0
      vim.g.vimtex_view_skim_sync = 1
      vim.g.vimtex_view_skim_reading_bar = 1
      vim.g.vimtex_syntax_enabled = 0
      vim.g.vimtex_syntax_conceal_disable = 1
      vim.g.conceallevel = 2
      vim.g.vimtex_quickfix_enabled = 1
      vim.g.vimtex_compiler_latexmk = {
        options = {
          "-pdf",
          "-shell-escape",
          "-verbose",
          "-file-line-error",
          "-synctex=1",
          "-interaction=nonstopmode",
        },
      }
    end,
  },
  { "jakewvincent/texmagic.nvim" },
}
