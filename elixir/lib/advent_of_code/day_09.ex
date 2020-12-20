defmodule AdventOfCode.Day09 do
  def arrange(0, _), do: [[]]
  def arrange(_, []), do: []

  def arrange(n, [x | xs]) do
    for(l <- arrange(n - 1, xs), do: [x | l]) ++ arrange(n, xs)
  end

  def subarrays(list) do
    Enum.reduce(1..length(list), [], fn n, acc -> acc ++ arrange(n, list) end)
  end

  def find_wrong(list) do
    [target | rest] = Enum.reverse(list)
    arran = arrange(2, rest)
    !Enum.find(arran, fn [x, y] -> x + y == target end)
  end

  def part1(input) do
    input
    |> Enum.chunk_every(26, 1, :discard)
    |> Enum.filter(&find_wrong(&1))
    |> List.first()
    |> List.last()
  end

  def part2(input, num) do
    all_subbarrays = subarrays(input)
    index = all_subbarrays |> Enum.map(&Enum.sum(&1)) |> Enum.find_index(fn x -> x == num end)
    required_subarray = Enum.at(all_subbarrays, index)
    Enum.max(required_subarray) + Enum.min(required_subarray)
  end

  def parse_input(input_txt) do
    input_txt |> String.split("\n") |> Enum.map(&String.to_integer(&1))
  end
end
