return {
  {
    "lervag/vimtex",
    lazy = true,
    config = function()
      vim.g.vimtex_view_method = "sioyek"
      vim.g.maplocalleader = " "
      vim.g.conceallevel = 2
      vim.g.vimtex_view_skim_sync = 1
      vim.cmd([[function! s:write_server_name() abort
  let nvim_server_file = (has('win32') ? $TEMP : '/tmp') . '/vimtexserver.txt'
  call writefile([v:servername], nvim_server_file)
endfunction

augroup vimtex_common
  autocmd!
  autocmd FileType tex call s:write_server_name()
augroup END
]])
    end,
  },
}
