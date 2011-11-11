
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

# This runs docco on the source file and moves the output into a deeper level
# than just ./docs.
def docco(src_file)
  sh %{docco #{src_file}}

  basename = File.basename(src_file, '.hs') + '.html'
  tmp = File.join("docs", basename)

  parts = Pathname.new(src_file).each_filename.to_a
  parts[parts.index('src')] = 'docs'
  dest = File.join(*parts)[0..-4] + '.html'

  if tmp == dest
    # If the tmp and dest are the same, then change dest's filename to
    # index.html and only copy it.
    dest = File.join(File.dirname(tmp), 'index.html')
    FileUtils.cp(tmp, dest, :verbose => true)
  else
    dirname = File.dirname(dest)
    FileUtils.mkdir_p(dirname)
    FileUtils.mv(tmp, dest, :verbose => true)
    FileUtils.cp('docs/docco.css', File.join(dirname, 'docco.css'),
                 :verbose => true)
  end
end

desc 'Creates docs using docco (must be on path).'
task :docs do
  hs_files = File.join('src', '**', '*.hs')
  Dir.glob(hs_files).each { |filename| docco(filename) }
end

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

namespace :hs do
  desc 'This builds the Haskell part of the project.'
  task :build do
    sh %{cabal build}
  end
end

namespace :release do
  desc 'Cleans up everything and configures for release.'
  task :build => [:clean, 'release:config'] do
    sh %{cabal build}
  end

  desc 'Configures the project for development.'
  task :config do
    sh %{cabal configure}
  end
end


