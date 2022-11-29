if (darg = ARGV.find { |a| a.to_i != 0 })
  ARGV.delete(darg)
  day = Integer(darg)
else
  require 'date'
  day = Date.today.day
  puts "guessing day = #{day}"
end

daypad = day.to_s.rjust(2, ?0)
name = "#{daypad}_#{ARGV[0] || 'tobenamed'}"

File.open(__dir__ + '/adventofcode.cabal', ?a) { |f|
  f.puts
  f.puts <<~CABAL
  executable #{name}
    import: bin, flags
    build-depends: base, adventofcode2022, aocsplit, array, containers
    main-is:       #{name}.hs
  CABAL
}

system("sed -e 's/99/#{day}/' #{__dir__}/bin/00_template.hs > #{__dir__}/bin/#{name}.hs")
