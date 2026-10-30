cms = {"cmd", "ctrl"}

hs.hotkey.bindSpec({cms, "e"},
  function()
    hs.execute("/opt/homebrew/bin/emacsclient --eval '(emacs-everywhere)'")
end)
