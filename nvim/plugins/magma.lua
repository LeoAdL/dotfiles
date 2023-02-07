vim.api.nvim_set_keymap("n", "<leader>r", ":MagmaEvaluateOperator", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>rr", ":MagmaEvaluateLine<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>ro", ":MagmaShowOutput<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>rc", ":MagmaReevaluateCell<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>rd", ":MagmaDelete<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>rq", ":noautocmd MagmaEnterOutput<CR>", { noremap = true, silent = true })
-- vim.api.nvim_set_keymap("x", "<leader>rr", "<C-u>MagmaEvaluateVisual<CR>", { silent = true })

vim.cmd [[xnoremap <silent> <leader>r :<C-u>MagmaEvaluateVisual<CR>]]
vim.g.magma_image_provider = "kitty"
