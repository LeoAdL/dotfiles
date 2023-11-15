return { {
    "luukvbaal/nnn.nvim",
    config = function()
        require("nnn").setup({
            picker = {
                cmd = "nnn -a -e -Pp",
            },
            replace_netrw = "picker",
        })
    end
},
}
