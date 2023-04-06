return {
  {
    "lervag/vimtex",
    config = function()
      vim.g.vimtex_compiler_progname = "nvr"
      vim.g.vimtex_view_method = "skim"
      vim.g.maplocalleader = " "
      vim.g.vimtex_view_skim_reading_bar = 1
      vim.g.conceallevel = 2
      vim.g.vimtex_quickfix_enabled = 1
      vim.g.vimtex_compiler_latexmk = {
        options = { "-pdf", "-shell-escape", "-verbose", "-file-line-error", "-synctex=1", "-interaction=nonstopmode" },
      }
    end,
  },
}
