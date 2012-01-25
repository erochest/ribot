
require 'tempfile'
require 'fileutils'
require 'pathname'

task :default => :usage

task :usage do
  puts 'Try one of these commands:'
  sh %{rake -T}
end

desc 'Performs clean build.'
task :cleanbuild => [:clean,
                     :config,
                     :build]

desc 'Configures the project for development.'
task :config, [:target] do |t, args|
  target = args[:target] || 'development'

  case target
  when 'development'
    flags = %{-f development --enable-tests}
  else
    flags = ''
  end

  sh %{cabal configure #{flags}}
end

desc 'Cleans up everything.'
task :clean do
  sh %{cabal clean}
end

require 'zayin/rake/docco'
desc "This runs docco to create documentation (Zayin)."
Zayin::Rake::docco :docco, Dir.glob(File.join('src', '**', '*.hs'))

desc 'Creates the docs and commits onto the gh-pages branch.'
task :ghpages, [:msg] => [:docs] do |t, args|
  msg = args[:msg] || 'Updated docs.'

  FileUtils.rmtree("/tmp/ribot-docs", :verbose => true) if File.exists?("/tmp/ribot-docs")
  FileUtils.mv("docs", "/tmp/ribot-docs", :verbose => true)

  sh %{git stash}
  sh %{git checkout gh-pages}

  FileUtils.rmtree("docs", :verbose => true) if File.exists?("docs")
  FileUtils.mv("/tmp/ribot-docs", "docs", :verbose => true)

  sh %{git add --all docs}
  sh %{git commit -m "#{msg}"}

  puts "Don't forget to run these commands:"
  puts "    git checkout master"
  puts "    git stash pop"
end

desc 'Removes the logging database.'
task :nukedb do
  dirname = File.expand_path('~/.ribot')
  FileUtils::rmtree dirname, :verbose => true
end

desc 'Builds everything.'
task :build => ['hs:build']

desc 'Tests.'
task :test => :build do
  sh %{cabal test}
end

desc 'Runs ribot.'
task :run, [:args] => :build do |t, args|
  args.with_default(:args => '')
  sh %{./dist/build/ribot/ribot #{args[:args]}}
end

desc 'Runs ribot connecting to a test channel and using a test db.'
task :runtest => :build do
  Rake::Task['run'].invoke("-n ribot-test -c '#err1234567890' -d test.db")
end

desc 'Copies the binary to ~/bin.'
task :tobin => :build do
  FileUtils.cp('dist/build/ribot/ribot', File.expand_path('~/bin/ribot'),
               :verbose => true)
end

desc 'Creates the tags file.'
task :tags do
  FileUtils.rm('tags', :verbose => true)
  hs_tags = `find src -name '*.hs' | xargs hothasktags`
  File.open('tags', 'w') { |f| f.write(hs_tags) }
end

namespace :release do
  desc 'Cleans up everything and configures for release.'
  task :build => [:clean, 'release:config', 'hs:build', 'release:strip']

  desc 'Configures the project for development.'
  task :config do
    sh %{cabal configure}
  end

  desc 'This strips out extra comments and fluff from the binary.'
  task :strip do
    sh %{strip -p --strip-unneeded --remove-section=.comment -o ./ribot ./dist/build/ribot/ribot}
  end

  desc 'This copies the file to the ~/bin directory.'
  task :tobin => ['release:build', 'release:strip'] do
    FileUtils.cp('ribot', File.expand_path('~/bin/ribot'), :verbose => true)
  end
end

require 'zayin/rake/haskell'
Zayin::Rake::HaskellTasks.new

