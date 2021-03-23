desc "Generate character"
task :pdf, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    cmd = "caba exec csheet #{filename}"
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
    cmd = "cabal exec spell #{filename}"
    if system cmd then
        if not File.exist?("#{filename}.spells.tex") then
            puts "File not found: [#{filename}.spells.tex]"
        else
            if system "xelatex -output-directory='characters/#{args[:campaign]}/#{args[:name]}' #{filename}.spells.tex" then
                system "open #{filename}.spells.pdf"
            end
        end
    end
end