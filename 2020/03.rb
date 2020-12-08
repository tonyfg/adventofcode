def check_slope(right, down)
  map = File.readlines('03_input.txt').map(&:strip).each_slice(down).map(&:first)
  width = map.first.length
  map.each_with_index.reduce(0) do |acc, (s, i)|
    s[(i * right) % width] == '#' ? acc + 1 : acc
  end
end

# Part 1
check_slope(3, 1) # => 237

# Part 2
[[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]].reduce(1) do |mult, (right, down)|
  mult * check_slope(right, down)
end # => 2106818610
