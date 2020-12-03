defmodule AdventOfCode.Day03 do
  defmodule Grid do
    defstruct [:tree, :width, :height]
  end

  def addPos({x1, y1}, {x2, y2}), do: {x1 + x2, y1 + y2}

  def isTree(%Grid{tree: tree, width: w}, {x, y}) do
    MapSet.member?(tree, {rem(x, w), y})
  end

  def isInBound(%Grid{height: h}, {_, y}) do
    y >= 0 and y < h
  end

  def encounterTree(grid, step) do
    Stream.iterate({0, 0}, &addPos(&1, step))
    |> Enum.take_while(&isInBound(grid, &1))
    |> Enum.filter(&isTree(grid, &1))
    |> length
  end

  def part1(grid) do
    encounterTree(grid, {3, 1})
  end

  def part2(grid) do
    [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}]
    |> Enum.map(&encounterTree(grid, &1))
    |> Enum.reduce(fn x, ac -> x * ac end)
  end

  def inputToGrid(lines) do
    max = 100_000_000

    treePositions =
      lines
      |> Stream.zip(0..max)
      |> Stream.flat_map(fn {line, y} ->
        to_charlist(line)
        |> Stream.zip(0..max)
        |> Stream.map(fn {c, x} -> {{x, y}, [c]} end)
      end)
      |> Stream.filter(fn {_, x} -> x == '#' end)
      |> Stream.map(fn {x, _} -> x end)
      |> MapSet.new()

    %Grid{tree: treePositions, height: length(lines), width: String.length(hd(lines))}
  end
end
