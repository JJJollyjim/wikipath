titles_to_ids = {} of String => String

vecs = File.open("/scratch/linkVecsB", "w")
vecmap = File.open("/scratch/vecmapB", "w")


links = File.open("/scratch/idToIdLinksSorted").each_line.map do |line|
	from, to = line.split(" ")
	{from.to_u32, to.to_u32}
end

link_targets = links.chunk { |(from, to)| from } 
	.map { |(key, vals)| {key, vals.map { |(_, val)| val }} }

zero = StaticArray(UInt8, 4).new(0_u8).to_slice
nonexistant = StaticArray(UInt8, 4).new(0xFF_u8).to_slice

times_i_did_the_thing = 0

vecs_ints_written = 0_u32
last_from = -1
link_targets.each do |(from, tos)|
       if from > 53934078
	       puts from
	       puts tos
       end

	# nonexisteant articles have links-ptr of 0xFFFFFFFF
	skipped = from - last_from - 1
	skipped.times do
		vecmap.write(nonexistant)
		times_i_did_the_thing += 1
	end

	# ptr to vec of links
	vecmap.write_bytes(vecs_ints_written.as(UInt32), IO::ByteFormat::LittleEndian)
	times_i_did_the_thing += 1

	# vec of links
	tos.each do |to|
		vecs.write_bytes(to.as(UInt32), IO::ByteFormat::LittleEndian)
		vecs_ints_written += 1
	end

	# delimit vec of links
	vecs.write(zero)
	vecs_ints_written += 1

	if times_i_did_the_thing % 10000 == 0
		puts "prog #{times_i_did_the_thing * 100 / 53934078}%"
	end

	last_from = from
end

vecs.close
vecmap.close

puts "dtt #{times_i_did_the_thing}"
