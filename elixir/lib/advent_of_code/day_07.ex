defmodule AdventOfCode.Day07 do
  def search_bag(key, map) do
    children = Map.get(map, key) |> Enum.map(fn {_, x} -> x end)
    "shiny gold" in children or Enum.any?(children, &search_bag(&1, map))
  end

  def count_bag({num, bag}, map) do
    children = Map.get(map, bag)
    num + num * Enum.reduce(children, 0, fn x, acc -> count_bag(x, map) + acc end)
  end

  def part1(txt) do
    bags = txt |> input_to_map

    Enum.reduce(Map.keys(bags), [], fn x, acc ->
      if search_bag(x, bags), do: [x] ++ acc, else: acc
    end)
  end

  def part2(txt) do
    bags = txt |> input_to_map
    gold_children = Map.get(bags, "shiny gold")
    Enum.reduce(gold_children, 0, fn x, acc -> count_bag(x, bags) + acc end)
  end

  @regex ~r/((?<s_bag>\w+ \w+) \w+) contain (?<bags>((\d \w+ \w+ \w+)(, )?)*|(\w+ \w+ \w+))?/
  @bags_regex ~r/(((?<bag>\d \w+ \w+) \w+)(, )*)?/
  @split_regex ~r/(?<num>\d)/
  def input_to_map(txt) do
    txt
    |> String.split("\n")
    |> Enum.map(&Regex.named_captures(@regex, &1))
    |> Enum.map(fn %{"s_bag" => sbag, "bags" => bags} ->
      bags =
        Regex.scan(@bags_regex, bags, capture: :all_names)
        |> Enum.filter(&(&1 != [""]))

      {sbag, List.flatten(bags)}
    end)
    |> Enum.map(fn {sbag, x} ->
      {sbag,
       Enum.map(x, fn x ->
         [num, bag] = Regex.split(@split_regex, x, include_captures: true, trim: true)
         {String.to_integer(num), String.trim(bag)}
       end)}
    end)
    |> Map.new()
  end
end
