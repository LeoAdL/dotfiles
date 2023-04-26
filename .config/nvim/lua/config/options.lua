-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.opt.conceallevel = 2 -- hide * markup for bold and italic
vim.opt.conceallevel = 2 -- hide * markup for bold and italic
vim.opt.autochdir = true
vim.opt.wrap = true
local home = os.getenv("HOME")

if home == nil then
    return
end

local path = table.concat({
    "/usr/share/lua/5.1/?.lua",
    "/usr/share/lua/5.1/?/init.lua",
    "/usr/lib/lua/5.1/?.lua",
    "/usr/lib/lua/5.1/?/init.lua",
    "./?.lua",
    "./?/init.lua",
    "~/.luarocks/share/lua/5.1/?.lua",
    "~/.luarocks/share/lua/5.1/?/init.lua",
}, ";")

local cpath = table.concat({
    "/usr/lib/lua/5.1/?.so",
    "/usr/lib/lua/5.1/loadall.so",
    "./?.so",
    "~/.luarocks/lib/lua/5.1/?.so",
}, ";")

package.path = path:gsub("~", home)
package.cpath = cpath:gsub("~", home)

if vim.g.neovide then
    vim.o.guifont = "Iosevka:h16"
    -- Helper function for transparency formatting
    local alpha = function()
        return string.format("%x", math.floor(255 * vim.g.transparency or 0.8))
    end
    -- g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
    vim.g.neovide_refresh_rate = 60
    vim.g.neovide_remember_window_size = true
    vim.g.neovide_input_macos_alt_is_meta = true
    vim.g.neovide_cursor_antialiasing = true
end
