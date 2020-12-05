defmodule AdventOfCode.Day05 do
  def find_rows("", lo, _), do: lo

  def find_rows(<<x::utf8, rest::binary>>, lo, hi) do
    if x == ?F do
      find_rows(rest, lo, div(lo + hi, 2))
    else
      find_rows(rest, div(lo + hi, 2) + 1, hi)
    end
  end

  def find_columns("", lo, _), do: lo

  def find_columns(<<x::utf8, rest::binary>>, lo, hi) do
    if x == ?L do
      find_columns(rest, lo, div(lo + hi, 2))
    else
      find_columns(rest, div(lo + hi, 2) + 1, hi)
    end
  end

  def get_seat_id(bpas) do
    find_rows(String.slice(bpas, 0..6), 0, 127) * 8 + find_columns(String.slice(bpas, 7..9), 0, 7)
  end

  def part1(input) do
    input |> Enum.map(&get_seat_id(&1)) |> Enum.max()
  end

  def part2(input) do
    seatIds = input |> Enum.map(&get_seat_id(&1)) |> Enum.sort()
    Enum.reduce(seatIds, fn x, acc -> if x - acc == 1, do: x, else: acc end) + 1
  end
end
