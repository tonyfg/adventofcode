require 'digest'

i = 'ckczppom'

# part 1
j = 1
loop do
  break if Digest::MD5.hexdigest(i + j.to_s)[0..4] == '0'*5
  j += 1
end
j

# part 2
j = 1
loop do
  break if Digest::MD5.hexdigest(i + j.to_s)[0..4] == '0'*6
  j += 1
end
j
