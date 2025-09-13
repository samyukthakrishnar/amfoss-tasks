defmodule HollowPyramid do
  # Main function to run the program
  def run do
    # Read the number of rows from input.txt
    case File.read("input.txt") do
      {:ok, content} ->
        rows = content |> String.trim() |> String.to_integer()
        print_hollow_pyramid(rows)

      {:error, reason} ->
        IO.puts("Error reading file: #{reason}")
    end
  end

  # Function to print the pyramid pattern
  def print_hollow_pyramid(rows) do
    # Loop for each row
    for i <- 1..rows do
      # Print leading spaces for centering
      spaces = String.duplicate(" ", rows - i)

      # Build the star row with conditions for hollow shape
      stars =
        for k <- 1..(2 * i - 1) do
          if i == 1 or i == rows do
            "*"
          else
            if k == 1 or k == (2 * i - 1) do
              "*"
            else
              " "
            end
          end
        end
        |> Enum.join("")

      # Print the complete line
      IO.puts("#{spaces}#{stars}")
    end
  end
end

# Run the program
HollowPyramid.run()
