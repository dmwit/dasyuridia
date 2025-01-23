local MAX_ARRAY_READ_SIZE = 16

local function parse_message(line)
	local waddr0, wval0, waddr1, wval1, raddr0, rsz0, raddr1, rsz1 =
		line:match('^(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)$')
	if waddr0 == nil then
		io.write('ERR: malformed arguments request: "' .. line .. '"\n')
		return
	end
	waddr0, wval0, waddr1, wval1, raddr0, rsz0, raddr1, rsz1 = tonumber(waddr0), tonumber(wval0), tonumber(waddr1), tonumber(wval1), tonumber(raddr0), tonumber(rsz0), tonumber(raddr1), tonumber(rsz1)
	if rsz0 + rsz1 > MAX_ARRAY_READ_SIZE then
		local msg = "WARNING: read size may cause missed deadlines; recommended combined size of reads is %d, but saw %d + %d = %d\n"
		io.write(msg:format(MAX_ARRAY_READ_SIZE, rsz0, rsz1, rsz0+rsz1))
	end
	if waddr0 ~= 0xfffe then memory.writebyte(waddr0, wval0) end
	if waddr1 ~= 0xffff then memory.writebyte(waddr1, wval1) end
	io.write("dasyuridia:")
	for i=1,rsz0 do
		io.write(' ' .. memory.readbyte(raddr0+i-1))
	end
	for i=1,rsz1 do
		io.write(' ' .. memory.readbyte(raddr1+i-1))
	end
	io.write('\n')
	io.flush()
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
		if poll_stdin() then parse_message(io.read()) end
	end)
end
