titles_to_ids = {} of String => String

File.open("/scratch/idsAndTitles").each_line.with_index do |line, i|
	id, title = line.split(" ")
	titles_to_ids[title] = id
	if i % 1_000_000 == 0
		STDERR.puts i
	end
end

File.open("/scratch/idToTitleLinks").each_line do |line|
	id, title = line.split(" ")
	target_id = titles_to_ids.fetch(title, nil)

	if !target_id.nil?
		puts id + " " + target_id
	end
end
