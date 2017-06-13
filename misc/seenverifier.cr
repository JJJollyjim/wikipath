set = Set(UInt32).new
lns = File.open("/scratch/seens").each_line.with_index.each do |line, i|
	seen, ns = line.split(" ")
	n = ns.to_u32
	if seen == "seen"
		if !(set.includes? n)
			puts "FAILa #{i}"
		end
	elsif seen == "notseen"
		if set.includes? n
			puts "FAILb #{i}"
		end
		set << n
	else
		puts "FAILc #{i}"
	end
end
