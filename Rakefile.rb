desc "Generate character"
task :pdf, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    cmd = "./dist-newstyle/build/x86_64-osx/ghc-8.10.1/csheet-0.1.0.0/x/csheet/build/csheet/csheet #{filename}"
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
task :book, [:page] do |t, args|
    res = system "xelatex characters/phandalin/thomas/SpellBook.#{args[:page]}.tex"
    system "open Spellbook.#{args[:page]}.pdf" if res
end

task :spell, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    cmd = "./dist-newstyle/build/x86_64-osx/ghc-8.10.1/csheet-0.1.0.0/x/spell/build/spell/spell #{filename}"
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