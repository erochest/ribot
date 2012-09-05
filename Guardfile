
guard :shell, all_on_start: true do
  watch  /(src|tests)\/.*\.(l?hs)$/  do |m|
    `cabal build && cabal test`
  end
end

