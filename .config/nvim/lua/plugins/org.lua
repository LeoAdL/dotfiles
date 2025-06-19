return {
    'nvim-orgmode/orgmode',
    event = 'VeryLazy',
    enabled = false,
    dependencies = {
        "nvim-orgmode/org-bullets.nvim",
    },
    config = function()
        -- Setup orgmode
        require('orgmode').setup({
            org_agenda_files = '~/org/**/*',
            org_default_notes_file = '~/org/refile.org',
        })

        require('org-bullets').setup()
    end,
}
