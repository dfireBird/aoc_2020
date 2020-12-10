defmodule AdventOfCode.Day10 do
  def count(n, [], pre), do: 0

  def count(n, [hd | tl], pre) do
    if hd - pre == n do
      1 + count(n, tl, hd)
    else
      0 + count(n, tl, hd)
    end
  end

  def ways([], _, paths), do: paths

  def ways([hd | tl], ports, paths) do
    children = Map.get(ports, hd)

    new_paths =
      Enum.reduce(children, paths, fn child, acc ->
        child_val = Map.get(acc, child)
        Map.update!(acc, hd, &(&1 + child_val))
      end)

    ways(tl, ports, new_paths)
  end

  def part1(input) do
    count(1, input, 0) * (count(3, input, 0) + 1)
  end

  def part2(input) do
    maximum = Enum.max(input) + 3
    list = [0 | [maximum | input]] |> Enum.sort() |> input_to_map
    topo_sort = list |> Enum.map(fn {x, _} -> x end) |> Enum.sort() |> Enum.reverse()

    number_map =
      topo_sort
      |> Enum.zip(for _ <- 0..length(topo_sort), do: 0)
      |> Map.new()
      |> Map.update!(maximum, fn _ -> 1 end)

    Map.get(ways(topo_sort, list, number_map), 0)
  end

  def input_to_map(list) do
    list
    |> Enum.map(fn x -> {x, 1..3 |> Enum.map(&(&1 + x)) |> Enum.filter(&(&1 in list))} end)
    |> Map.new()
  end

  def parse_input(txt) do
    txt |> String.split("\n") |> Enum.map(&String.to_integer(&1)) |> Enum.sort()
  end
end
