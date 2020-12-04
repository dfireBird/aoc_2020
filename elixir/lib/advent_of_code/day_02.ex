defmodule AdventOfCode.Day02 do
  def input_decode(inputs) do
    inputs
    |> Enum.map(&String.split(&1, ": "))
    |> Enum.map(fn [pol, pass] ->
      [r, char] = String.split(pol)
      [lo, hi] = String.split(r, "-") |> Enum.map(&String.to_integer/1)
      {lo, hi, char, pass}
    end)
  end

  def count(fun, list) do
    length(Enum.filter(list, fun))
  end

  def xor_bool(a, b) do
    (a and not b) or (not a and b)
  end

  def part1(list) do
    count(
      fn {lo, hi, c, pass} ->
        count(&(&1 === hd(to_charlist(c))), to_charlist(pass)) in lo..hi
      end,
      list
    )
  end

  def part2(list) do
    count(
      fn {p1, p2, c, pass} ->
        xor_bool(String.at(pass, p1 - 1) == c, String.at(pass, p2 - 1) == c)
      end,
      list
    )
  end
end
