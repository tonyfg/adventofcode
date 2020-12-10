PREAMBLE_SIZE = 25
nums = File.readlines('09_input.txt').map(&:to_i)

# Part 1
first_invalid_num = PREAMBLE_SIZE.upto(nums.length - 1).each do |i|
  n = nums[i]
  preamble = nums[i - PREAMBLE_SIZE ... i]
  break n if !preamble.product(preamble).map(&:sum).include?(n)
end
# => 552655238


# Part 2
0.upto(nums.length - 1) do |i|
  span = [nums[i], nums[i + 1]]
  next_in_span = 2

  loop do
    break if span.sum >= first_invalid_num
    span << nums[i + next_in_span]
    next_in_span += 1
  end

  break span.min + span.max if span.sum == first_invalid_num
end
# => 70672245
