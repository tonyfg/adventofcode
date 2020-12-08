n = 277678

# part 1
def fonix(n)
  dirs = [[1, 0], [0, 1], [-1, 0], [0, -1]]
  (n / 2.0).ceil
           .times
           .flat_map { |i| [dirs[i % 4]] * (i / 2.0 + 0.5).floor }
           # .reduce([0,0]) { |a, e| [a[0] + e[0], a[1] + e[1]] }
           # .map(&:abs).sum
end
