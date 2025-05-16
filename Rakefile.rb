desc "Generate character"
task :pdf, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    cmd = "cabal exec cexe -- -j #{filename}.yml -p #{filename}.pdf"
    # puts cmd
    res = system cmd
    system "open #{filename}.pdf" if res
    # system "open #{filename}.spells.html" if res
end
task :pdf => [:build]

task :spell, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    system "open #{filename}.spells.html"
end

desc "Build generator"
task :build do
    `cabal build`
end
task :default => [:build]

desc "Convert DnD Beyond PDF to custom PDF"
task :dndb, [:inputpdf, :template, :outputpdf] do |t, args|
    puts "Do the thing"
    system "cabal exec pdf -- -p #{args[:inputpdf]} > temp.json"
    system "cabal exec cexe -- -j temp.json -p #{args[:outputpdf]} -t #{args[:template]}"
    # Delete temp.json
end