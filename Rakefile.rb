desc "Generate character"
task :pdf, [:campaign, :name, :level] do |t, args|
    filename = "characters/#{args[:campaign]}/#{args[:name]}/#{args[:name]}.#{args[:level]}"
    cmd = ".stack-work/dist/x86_64-osx/Cabal-2.2.0.1/build/csheet/csheet #{filename}"
    res = system cmd
    system "open #{filename}.pdf" if res
end
task :pdf => [:build]

desc "Build generator"
task :build do
    `stack build`
end
task :default => [:build]