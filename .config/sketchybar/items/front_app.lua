local colors = require("colors")
local settings = require("settings")

-- Events that get pushed by yabai
sbar.add("event", "window_focus")
sbar.add("event", "title_change")

local front_app = sbar.add("item", "front_app", {
	display = "active",
	icon = {
		background = {
			drawing = true,
			image = {
				border_width = 2,
				border_color = colors.black,
			},
		},
	},

	background = {
		color = colors.bg1,
		border_color = colors.black,
		border_width = 1,
	},
	label = {
		color = colors.white,
		font = {
			style = settings.font.style_map["Black"],
			size = 12.0,
		},
	},
	updates = true,
})

-- Animate app icon back to 1.0
local function end_bounce_animation()
	sbar.animate("tanh", 15, function()
		front_app:set({
			icon = {
				background = {
					image = { scale = 1.0 },
				},
			},
		})
	end)
end

-- Make app icon slightly bigger before returning back to regular size
local function start_bounce_animation()
	sbar.animate("tanh", 15, function()
		front_app:set({
			icon = {
				background = {
					image = { scale = 1.2 },
				},
			},
		})
	end)
	-- Short delay so that full animation can occur
	sbar.exec("sleep 0.25 && echo 'finishing bounce'", end_bounce_animation)
end

front_app:subscribe("front_app_switched", function(env)
	front_app:set({
		icon = { background = { image = "app." .. env.INFO } },
		label = { string = env.INFO },
	})
	set_window_title()
	start_bounce_animation()
end)

front_app:subscribe("space_change", function()
	set_window_title()
	start_bounce_animation()
end)

front_app:subscribe("window_focus", function()
	set_window_title()
	start_bounce_animation()
end)

front_app:subscribe("title_change", function()
	set_window_title()
end)

front_app:subscribe("mouse.clicked", function(env)
	sbar.trigger("swap_menus_and_spaces")
end)
