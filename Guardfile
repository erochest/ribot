
guard :shell, all_on_start: true do
  watch  /(src|tests)\/.*\.(l?hs)$/  do |m|
    `cabal-dev build && cabal-dev test`
  end
end

