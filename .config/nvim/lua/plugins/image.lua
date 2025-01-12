local trigger = false
if vim.loop.os_uname().sysname == 'Darwin'
then
    trigger = true
end
return {
    "3rd/image.nvim",
    enabled = trigger,
    lazy = true,
    rocks = {
        hererocks = true, -- recommended if you do not have global installation of Lua 5.1.
    },
    config = true
}
