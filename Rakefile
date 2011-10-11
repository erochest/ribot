
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


