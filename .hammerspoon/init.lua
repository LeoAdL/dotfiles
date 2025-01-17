cms = {"cmd", "ctrl"}

hs.hotkey.bindSpec({cms, "e"},
  function()
    hs.execute("/etc/profiles/per-user/leoap/bin/emacsclient --eval '(emacs-everywhere)'")
end)
