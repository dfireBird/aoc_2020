defmodule AdventOfCode.Day06 do
  def count(set) do
    Enum.reduce(set, 0, &(&2 + MapSet.size(&1)))
  end

  def part1(input) do
    input
    |> String.split("\n\n")
    |> Enum.map(&to_charlist(&1))
    |> Enum.map(&MapSet.new(&1))
    |> Enum.map(&MapSet.delete(&1, 10))
    |> count
  end

  def part2(input) do
    input
    |> String.split("\n\n")
    |> Enum.map(&String.split(&1))
    |> Enum.map(fn x -> Enum.map(x, &to_char_list(&1)) end)
    |> Enum.map(fn x -> Enum.map(x, &MapSet.new(&1)) end)
    |> Enum.map(fn x -> Enum.reduce(x, &MapSet.intersection(&1, &2)) end)
    |> count
  end
end
