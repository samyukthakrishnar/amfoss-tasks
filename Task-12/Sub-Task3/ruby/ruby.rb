def ruby(n)
  (1...n).each do |i|
    print " " * (n - i)
    if i == 1
      puts "*"
    else
      puts "*" + " " * (2*i - 3) + "*"
    end
  end
  puts "*" * (2*n - 1)
end

n = File.read("input.txt").strip.to_i
ruby(n)
