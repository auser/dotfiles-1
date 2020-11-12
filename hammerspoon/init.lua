hyper = require('hyper')
-- require("hyper")
-- require("reload")
require("meta")
require("test")

-- hs.loadSpoon("Lunette")
-- spoon.Lunette:bindHotkeys()
-- local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0.1
spoon.MiroWindowsManager:bindHotkeys({
  up = {hyper, "up"},
  right = {hyper, "right"},
  down = {hyper, "down"},
  left = {hyper, "left"},
  fullscreen = {hyper, "f"}
})

-- reload
local function reload_config()
  hs.reload()
  hs.alert.show("Config Reloaded")
end

function reloadConfig(files)
    local doReload = false
    for _, file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        -- hs.reload()
        reload_config()
    end
end


hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

-- Reload config
hs.hotkey.bind(hyper, "R", reload_config)
hs.urlevent.bind("reloadConfig", reload_config)
