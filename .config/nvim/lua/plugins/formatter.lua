return {
  "mhartington/formatter.nvim",
  config = function()
    -- Utilities for creating configurations
    local util = require "formatter.util"

    -- Provides the Format, FormatWrite, FormatLock, and FormatWriteLock commands
    require("formatter").setup {
      -- Enable or disable logging
      logging = true,
      -- Set the log level
      log_level = vim.log.levels.WARN,
      -- All formatter configurations are opt-in
      filetype = {
        python = {
          require("formatter.filetypes.python").black,
        },
        yaml = { require("formatter.filetypes.yaml").yamlfmt },
      }
    }
    vim.api.nvim_create_autocmd({ "BufWritePost" }, {
      command = "FormatWrite"
    })
  end,
}