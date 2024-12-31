-- [NOTE: Crowd Control compatibility]
-- The Crowd Control patch has to do all this stuff on the original NES
-- hardware, so it has some pretty sharp memory limits. Since this script
-- exists primarily as a way to test applications that want to interact with a
-- CC-ified game without needing to actually patch a game, buy an N8, and wire
-- up a connection, we mimic those limits. This makes it easier to notice if
-- you try to do something that wouldn't work on the original hardware.
-- Generally, however, this script actually is written to work even if you
-- exceed those limits. So if you don't care about the original hardware, and
-- just want to interact with dasyuridia+fceux, feel free to delete the checks
-- that directed you to this note. If anything breaks, report a bug!

local PROTOCOL_VERSION = 0
local MAX_ARRAY_READ_SIZE = 16
local MAX_ARRAY_WRITE_SIZE = 16
local MAX_FREEZE_COUNT = 5

local function exec_read(msg_id, args)
	if args:match('^[0-9][0-9 ]+$') == nil then
		io.write('ERR: malformed address list in read arguments: "' .. args .. '"\n')
		return
	end
	io.write(msg_id .. ' read')
	for addr in args:gmatch('%d+') do
		io.write(' ' .. memory.readbyte(tonumber(addr)))
	end
	io.write('\n')
end

local function exec_array_read(msg_id, args)
	local size, base = args:match('^(%d+)%s(%d+)$')
	if size == nil then
		io.write('ERR: malformed size or base address in read_array arguments: "' .. args .. '"\n')
		return
	end
	size, base = tonumber(size), tonumber(base)
	-- see [NOTE: Crowd Control compatibility]
	if size > MAX_ARRAY_READ_SIZE then
		io.write('ERR: read size is too large; only arrays of size up to  ' .. MAX_ARRAY_READ_SIZE .. ' are supported\n')
		return
	end
	io.write(msg_id .. ' array_read')
	for i=1,size do
		io.write(' ' .. memory.readbyte(base+i-1))
	end
	io.write('\n')
end

local function exec_write(msg_id, args)
	if args:match('^[0-9][0-9 ]+$') == nil then
		io.write('ERR: malformed address list in write arguments: "' .. args .. '"\n')
		return
	end
	for addr, val in args:gmatch('(%d+)%s(%d+)') do
		memory.writebyte(tonumber(addr), tonumber(val))
	end
	io.write(msg_id .. ' write\n')
end

local function exec_array_write(msg_id, args)
	if args:match('^[0-9][0-9 ]+$') == nil then
		io.write('ERR: malformed address list in array_write arguments: "' .. args .. '"\n')
		return
	end
	local base, vals = args:match("^(%d+)%s(.*)$")
	base = tonumber(base)
	-- TODO: complain if vals has more than MAX_ARRAY_WRITE_SIZE elements and
	-- add a reference to [NOTE: Crowd Control compatibility]
	local offset = 0
	for val in vals:gmatch("%d+") do
		memory.writebyte(base+offset, tonumber(val))
		offset = offset + 1
	end
	io.write(msg_id .. " array_write\n")
end

local function exec_null(msg_id, args)
	if args ~= "" then
		io.write('ERR: unexpected arguments to null message: "' .. args .. '"\n')
		return
	end
	io.write(msg_id .. " null\n")
end

local function exec_version(msg_id, args)
	if args ~= "" then
		io.write('ERR: unexpected arguments to version message: "' .. args .. '"\n')
		return
	end
	io.write(msg_id .. ' version '
		.. PROTOCOL_VERSION .. ' '
		.. MAX_ARRAY_READ_SIZE .. ' '
		.. MAX_ARRAY_WRITE_SIZE .. ' '
		.. MAX_FREEZE_COUNT .. '\n')
end

local freezes = {size=0}

local function large_write(address, value, size)
	for i = address+size-1, address, -1 do
		memory.writebyte(i, value % 256)
		value = math.floor(value / 256)
	end
end

local function large_read(address, size)
	local value = 0
	for i = address, address+size-1 do
		value = 256 * value + memory.readbyte(i)
	end
	return value
end

-- these don't work for truly large reads and writes -- e.g. probably 4 bytes
-- is fine but 8 is not -- but let's not fix that problem until it's actually a
-- problem
local WRITE_MODE_TABLE =
	{ write = function(self) large_write(self.write_address, self.write_value, self.write_size) end
	, inc = function(self) large_write(self.write_address, large_read(self.write_address, self.write_size) + self.write_value, self.write_size) end
	, dec = function(self) large_write(self.write_address, large_read(self.write_address, self.write_size) - self.write_value, self.write_size) end
	, masked = function(self)
	  	if self.inverse_mask == nil then
	  		self.inverse_mask = BIT(self.write_size*8) - 1 - self.mask
	  		-- just in case
	  		self.write_value = AND(self.mask, self.write_value)
	  	end
	  	local value = AND(inverse_mask, large_read(self.write_address, self.write_size))
	  	large_write(self.write_address, OR(value, self.write_value), self.write_size)
	  end
	}

local function comparator(f)
	return function(self)
		return f(large_read(self.compare_address, self.compare_size), self.compare_value)
	end
end

local COMPARE_MODE_TABLE =
	{ always        = function(self) return true end
	, equal         = comparator(function(a, b) return a == b end)
	, not_equal     = comparator(function(a, b) return a ~= b end)
	, greater       = comparator(function(a, b) return a >  b end)
	, greater_equal = comparator(function(a, b) return a >= b end)
	, less          = comparator(function(a, b) return a <  b end)
	, less_equal    = comparator(function(a, b) return a <= b end)
	}

local function schedule_freeze(msg_id, args)
	local write_size, compare_size, write_address, write_value, write_mask, write_mode, compare_address, compare_value, compare_mode
		= args:match('^(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%S+)%s(%d+)%s(%d+)%s(%S+)$')
	if write_size == nil then
		io.write("ERR: malformed freeze arguments: '" .. args .. "'\n")
		return
	end
	write_size, compare_size, write_address, write_value, write_mask, compare_address, compare_value
		= tonumber(write_size), tonumber(compare_size), tonumber(write_address), tonumber(write_value), tonumber(write_mask), tonumber(compare_address), tonumber(compare_value)

	-- see [NOTE: Crowd Control compatibility]
	-- in multibyte writes, the most significant byte has the lowest address
	if write_size ~= 1 then
		io.write("ERR: invalid write size in freeze arguments (saw " .. write_size .. " but expected 1)\n")
		return
	end
	-- see [NOTE: Crowd Control compatibility]
	-- in multibyte reads, the most significant byte has the lowest address
	if compare_size ~= 1 then
		io.write("ERR: invalid comparison size in freeze arguments (saw " .. compare_size .. " but expected 1)\n")
		return
	end

	if write_mode == "unfreeze" or compare_mode == "unfreeze" then
		if freezes[write_address] ~= nil then
			freezes.size = freezes.size - 1
		end
		freezes[write_address] = nil
		success = true
	else
		local freeze =
			{ write_size=write_size
			, compare_size=compare_size
			, write_address=write_address
			, write_value=write_value
			, write_mask=write_mask
			, compare_address=compare_address
			, compare_value=compare_value
			, write=WRITE_MODE_TABLE[write_mode]
			, compare=COMPARE_MODE_TABLE[compare_mode]
			}
		if freeze.write == nil then
			io.write("ERR: malformed write mode '" .. write_mode .. "'; supported modes are ")
			for mode, _ in pairs(WRITE_MODE_TABLE) do
				io.write(mode .. ", ")
			end
			io.write("and unfreeze\n")
			return
		end
		if freeze.compare == nil then
			io.write("ERR: malformed compare mode '" .. compare_mode .. "'; supported modes are ")
			for mode, _ in pairs(COMPARE_MODE_TABLE) do
				io.write(mode .. ", ")
			end
			io.write("and unfreeze\n")
			return
		end
		if freezes[write_address] ~= nil then
			freezes.size = freezes.size - 1
		end
		-- see [NOTE: Crowd Control compatibility]
		success = freezes.size < MAX_FREEZE_COUNT
		if success then
			freezes[write_address] = freeze
			freezes.size = freezes.size + 1
		end
	end

	io.write(msg_id .. ' freeze ' .. tostring(success) .. '\n')
end

local function exec_freeze()
	for write_address, freeze in pairs(freezes) do
		if tonumber(write_address) ~= nil and freeze:compare() then
			freeze:write()
		end
	end
end

local action_table =
	{ read=exec_read
	, array_read=exec_array_read
	, write=exec_write
	, array_write=exec_array_write
	, null=exec_null
	, version=exec_version
	, freeze=schedule_freeze
	}

local function parse_message(line)
	local msg_id, action_string, argument_start = line:match("^(%S+)%s(%S+)()")
	if msg_id == nil then
		io.write('ERR: malformed message header in ' .. line .. '\n')
		return
	end
	local action_function = action_table[action_string]
	if action_function == nil then
		io.write('ERR: unknown action ' .. action_string .. ' requested by ' .. line .. '\n')
		return
	end
	action_function(msg_id, line:sub(argument_start + 1))
end

local INJECTION_POINTS =
	{ ['5b401f4ca7e1b12af3f29d8fc758dd2f'] = 0xb7e7 -- NTSC original
	, ['9d961a26f2104e461667c6a1cc57ab21'] = 0xb801 -- NTSC Rev A
	}

local uv = require("luv")
local stdin = uv.new_poll(0)
local again
stdin:start("r", function(err, events)
	if err == nil then
		again = true
		parse_message(io.read())
	end
end)
local INJECTION_POINT = INJECTION_POINTS[rom.gethash('md5')]
if INJECTION_POINT == nil then
	io.write("ERR: unrecognized hash " .. rom.gethash('md5') .. "; expected one of these:\n")
	for hash, _ in pairs(INJECTION_POINTS) do
		io.write(hash .. "\n")
	end
else
	memory.registerexec(INJECTION_POINT, function()
		again = true
		while(again) do
			again = false
			uv.run("nowait")
		end
		exec_freeze()
	end)
end
