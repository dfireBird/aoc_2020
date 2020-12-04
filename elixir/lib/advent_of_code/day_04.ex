defmodule AdventOfCode.Day04 do
  def is_valid_keys(passport) do
    Map.keys(passport) |> Enum.filter(&(&1 != "cid")) |> Enum.sort() ==
      Enum.sort(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])
  end

  def is_valid_byr(%{"byr" => byr}),
    do: String.length(byr) == 4 && String.to_integer(byr) in 1920..2002

  def is_valid_byr(_), do: false

  def is_valid_iyr(%{"iyr" => iyr}),
    do: String.length(iyr) == 4 && String.to_integer(iyr) in 2010..2020

  def is_valid_iyr(_), do: false

  def is_valid_eyr(%{"eyr" => eyr}),
    do: String.length(eyr) == 4 && String.to_integer(eyr) in 2020..2030

  def is_valid_eyr(_), do: false

  def is_valid_hgt(%{"hgt" => hyt}) do
    case String.split_at(hyt, String.length(hyt) - 2) do
      {cm, "cm"} -> String.to_integer(cm) in 150..193
      {inc, "in"} -> String.to_integer(inc) in 59..76
      _ -> false
    end
  end

  def is_valid_hgt(_), do: false

  def is_valid_hcl(%{"hcl" => hcl}), do: String.length(hcl) == 7 && hcl =~ ~r/#[0-9a-f]+/
  def is_valid_hcl(_), do: false

  def is_valid_ecl(%{"ecl" => ecl}), do: ecl in ~w(amb blu brn gry grn hzl oth)
  def is_valid_ecl(_), do: false

  def is_valid_pid(%{"pid" => pid}), do: String.length(pid) == 9 && Integer.parse(pid) != :error
  def is_valid_pid(_), do: false

  def is_valid_passport(p) do
    is_valid_keys(p) && is_valid_byr(p) && is_valid_iyr(p) && is_valid_eyr(p) && is_valid_hgt(p) &&
      is_valid_hcl(p) && is_valid_ecl(p) && is_valid_pid(p)
  end

  def part1(passports) do
    passports |> Enum.filter(&is_valid_keys(&1)) |> length
  end

  def part2(passports) do
    passports |> Enum.filter(&is_valid_passport(&1)) |> length
  end

  def input_to_map(txt) do
    txt
    |> String.split("\n\n")
    |> Enum.map(&String.split(&1))
    |> Enum.map(&parse_passport(&1))
    |> Enum.map(&Map.new(&1))
  end

  def parse_passport(pasport) do
    pasport |> Enum.map(&String.split(&1, ":")) |> Enum.map(fn [x, y] -> {x, y} end)
  end
end
