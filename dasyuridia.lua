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

local function reply(msg_id, msg_nm, args)
	io.write('dasyuridia: ' .. msg_id .. ' ' .. msg_nm)
	if args ~= "" and args ~= nil then
		io.write(' ' .. args)
	end
	io.write('\n')
	io.flush()
end

local function exec_read(msg_id, args)
	if args:match('^[0-9][0-9 ]+$') == nil then
		io.write('ERR: malformed address list in read arguments: "' .. args .. '"\n')
		return
	end
	local result = ''
	for addr in args:gmatch('%d+') do
		result = result .. ' ' .. memory.readbyte(tonumber(addr))
	end
	-- sub(2) to drop the initial leading space
	reply(msg_id, 'read', result:sub(2))
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
	local result = ''
	for i=1,size do
		result = result .. ' ' .. memory.readbyte(base+i-1)
	end
	-- sub(2) to drop the initial leading space
	reply(msg_id, 'array_read', result:sub(2))
end

local function exec_write(msg_id, args)
	if args:match('^[0-9][0-9 ]+$') == nil then
		io.write('ERR: malformed address list in write arguments: "' .. args .. '"\n')
		return
	end
	for addr, val in args:gmatch('(%d+)%s(%d+)') do
		memory.writebyte(tonumber(addr), tonumber(val))
	end
	reply(msg_id, 'write')
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
	reply(msg_id, 'array_write')
end

local function exec_null(msg_id, args)
	if args ~= "" then
		io.write('ERR: unexpected arguments to null message: "' .. args .. '"\n')
		return
	end
	reply(msg_id, 'null')
end

local function exec_version(msg_id, args)
	if args ~= "" then
		io.write('ERR: unexpected arguments to version message: "' .. args .. '"\n')
		return
	end
	reply(msg_id, 'version', ''
		.. PROTOCOL_VERSION .. ' '
		.. MAX_ARRAY_READ_SIZE .. ' '
		.. MAX_ARRAY_WRITE_SIZE .. ' '
		.. MAX_FREEZE_COUNT)
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

	reply(msg_id, 'freeze', tostring(success))
end

local function exec_freeze()
	for write_address, freeze in pairs(freezes) do
		if tonumber(write_address) ~= nil and freeze:compare() then
			freeze:write()
		end
	end
end

local function exec_write_read(msg_id, args)
	local waddr0, wval0, waddr1, wval1, raddr0, rsz0, raddr1, rsz1 =
		args:match('^(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)$')
	if waddr0 == nil then
		io.write('ERR: malformed arguments in write_read: "' .. args .. '"\n')
		return
	end
	waddr0, wval0, waddr1, wval1, raddr0, rsz0, raddr1, rsz1 = tonumber(waddr0), tonumber(wval0), tonumber(waddr1), tonumber(wval1), tonumber(raddr0), tonumber(rsz0), tonumber(raddr1), tonumber(rsz1)
	if rsz0 + rsz1 > MAX_ARRAY_READ_SIZE then
		local msg = "WARNING: read size may cause missed deadlines; recommended combined size of reads is %d, but saw %d + %d = %d\n"
		io.write(msg:format(MAX_ARRAY_READ_SIZE, rsz0, rsz1, rsz0+rsz1))
	end
	if waddr0 ~= 0xfffe then memory.writebyte(waddr0, wval0) end
	if waddr1 ~= 0xffff then memory.writebyte(waddr1, wval1) end
	local read_result = ""
	for i=1,rsz0 do
		read_result = read_result .. ' ' .. memory.readbyte(raddr0+i-1)
	end
	for i=1,rsz1 do
		read_result = read_result .. ' ' .. memory.readbyte(raddr1+i-1)
	end
	reply(msg_id, 'write_read', read_result:sub(2))
end

local action_table =
	{ read=exec_read
	, array_read=exec_array_read
	, write=exec_write
	, array_write=exec_array_write
	, null=exec_null
	, version=exec_version
	, freeze=schedule_freeze
	, write_read=exec_write_read
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

local poll_stdin = require("poll_stdin")
io.stdin:setvbuf("no")

local INJECTION_POINTS =
	{ ['5b401f4ca7e1b12af3f29d8fc758dd2f'] = 0xb7e7 -- NTSC original
	, ['9d961a26f2104e461667c6a1cc57ab21'] = 0xb801 -- NTSC Rev A
	}
local INJECTION_POINT = INJECTION_POINTS[rom.gethash('md5')]
if INJECTION_POINT == nil then
	io.write("ERR: unrecognized hash " .. rom.gethash('md5') .. "; expected one of these:\n")
	for hash, _ in pairs(INJECTION_POINTS) do
		io.write(hash .. "\n")
	end
else
	memory.registerexec(INJECTION_POINT, function()
		-- pessimistically assume we can only handle one message per frame
		if poll_stdin() then parse_message(io.read()) end
	end)
end
