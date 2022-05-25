desc "Generate character"
task :pdf, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    cmd = "cabal exec cexe -- -j #{filename}.json -p #{filename}.pdf"
    # puts cmd
    res = system cmd
    system "open #{filename}.pdf" if res
end
task :pdf => [:build]

desc "Build generator"
task :build do
    `cabal build`
end
task :default => [:build]

desc "Build spell book"
task :spell, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    cmd = "cabal exec spellhb -- -j #{filename}.json -o #{filename}.hb"
    system cmd
end

desc "Convert DnD Beyond PDF to custom PDF"
task :dndb, [:inputpdf, :template, :outputpdf] do |t, args|
    puts "Do the thing"
    system "cabal exec pdf -- -p #{args[:inputpdf]} > temp.json"
    system "cabal exec cexe -- -j temp.json -p #{args[:outputpdf]} -t #{args[:template]}"
    # Delete temp.json
end