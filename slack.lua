local insns = nil
memory.registerexec(0xb660, function()
	insns = debugger.getcyclescount()
end)
memory.registerexec(0x8005, function()
	local cycles = '-'
	if insns ~= nil then
		cycles = debugger.getcyclescount() - insns
	end
	io.write(emu.framecount() .. ',' .. cycles .. '\n')
	insns = nil
end)
