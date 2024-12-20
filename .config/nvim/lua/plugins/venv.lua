return {
    'linux-cultist/venv-selector.nvim',
    dependencies = { 'neovim/nvim-lspconfig', 'nvim-telescope/telescope.nvim', 'mfussenegger/nvim-dap-python' },
    branch = "regexp", -- This is the regexp branch, use this for the new version
    opts = {
        function(_, opts)
            if require("lazyvim.util").has("nvim-dap-python") then
                opts.dap_enabled = true
            end
        end,
        anaconda_base_path = '/opt/homebrew/Caskroom/miniforge/base/condabin/conda',
        anaconda_envs_path = '/opt/homebrew/Caskroom/miniforge/base/envs',
    },
    event = 'VeryLazy', -- Optional: needed only if you want to type `:VenvSelect` without a keymapping
    keys = {
        -- Keymap to open VenvSelector to pick a venv.
        { '<leader>vs', '<cmd>VenvSelect<cr>' },
        -- Keymap to retrieve the venv from a cache (the one previously used for the same project directory).
        { '<leader>vc', '<cmd>VenvSelectCached<cr>' },
    },
}
