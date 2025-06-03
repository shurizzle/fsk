---@class Mpv
---@field observe_property fun(name: string, type?: string, callback: fun(name: string, value: any))
---@field register_event fun(event: string, callback: fun())
---@field osd_message fun(message: string, duration?: number)
---@field get_property_native fun(name: string): any
---@field command_native fun(table): table|nil, string|nil

---@class Global
---@field mp Mpv

---@type Global
---@diagnostic disable-next-line: assign-type-mismatch
_G = _G

local skip = false

mp.observe_property("percent-pos", "number", function(
	_, --[[@type number|nil]]
	pos
)
	if skip or not pos or pos < 80 then
		return
	end
	local epno = mp.get_property_native("metadata/by-key/track")
	if not epno then
		skip = true
		return
	end
	local aid = mp.get_property_native("metadata/by-key/anilist_id")
	if not aid then
		skip = true
		return
	end
	local res, err = mp.command_native({
		name = "subprocess",
		args = { "aniscrobble", "scrobble", "-b", aid, epno },
		playback_only = false,
		capture_stdout = true,
		capture_stderr = true,
	})
	if not res and err then
		mp.osd_message(err)
		return
	end
	if res.error then
		mp.osd_message("Error: " .. tostring(res.error))
		return
	end
	if res.status ~= 0 then
		mp.osd_message("Error: " .. tostring(res.stderr))
		return
	end
	skip = true
end)

mp.register_event("file-loaded", function()
	skip = false
end)
