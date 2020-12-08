numbers = File.readlines('01_1_input.txt').map(&:to_i)

# naive
numbers.each do |m|
  n = numbers.reduce(nil) do |_ ,i|
    o = numbers.find { m + i + _1 == 2020 }
    break i * o if o
  end
  break m * n if n
end

# sort + binary search
sorted_numbers = numbers.sort
sorted_numbers.each do |m|
  n = numbers.reduce(nil) do |_ ,i|
    o = sorted_numbers.bsearch { m + i + _1 - 2000 }

    break i * o if o
  end

  break m * n if n
end
