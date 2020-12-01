defmodule AdventOfCode.Day01 do

  def arrange(0, _), do: [[]]
  def arrange(_, []), do: []
  def arrange(n, [x | xs]) do
    (for l <- arrange(n - 1, xs), do: [x | l]) ++ arrange(n, xs)
  end

  def part1(xs) do
    powerSetOfLength2 = arrange(2, xs)
    sumTo2020 = Enum.filter(powerSetOfLength2, fn [x,y] -> x + y == 2020 end)
    hd Enum.map(sumTo2020, fn [x,y] -> x * y end)
  end

  def part2(xs) do
    powerSetOfLength3 = arrange(3, xs)
    sumTo2020 = Enum.filter(powerSetOfLength3, fn [x,y,z] -> x + y + z == 2020 end)
    hd Enum.map(sumTo2020, fn [x,y,z] -> x * y * z end)
  end
end
