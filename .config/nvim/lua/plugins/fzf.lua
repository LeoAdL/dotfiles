return {
  { "vijaymarupudi/nvim-fzf" },
  {
    "ibhagwan/fzf-lua",
    -- optional for icon support
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("fzf-lua").setup({
        lsp = {
          -- make lsp requests synchronous so they work with null-ls
          async_or_timeout = 3000,
        },
      })
    end,
  },
}
