defmodule elixir do
  def draw(n) do
    for i <- 1..(n-1) do
      if i == 1 do
        IO.puts String.duplicate(" ", n - i) <> "*"
      else
        IO.puts String.duplicate(" ", n - i) <>
                "*" <>
                String.duplicate(" ", 2*i - 3) <>
                "*"
      end
    end
    IO.puts String.duplicate("*", 2*n - 1)
  end
end

{:ok, data} = File.read("input.txt")
n = String.trim(data) |> String.to_integer()
elixir.draw(n)
