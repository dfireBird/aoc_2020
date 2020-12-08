defmodule AdventOfCode.Day08 do
  def execute({:nop, num}, pos, acc) do
    {pos + 1, acc}
  end

  def execute({:jmp, num}, pos, acc) do
    {pos + num, acc}
  end

  def execute({:acc, num}, pos, acc) do
    {pos + 1, acc + num}
  end

  def run(ins, pos, acc, seen) do
    if MapSet.member?(seen, pos) do
      {:infinte_loop, acc, pos}
    else
      if pos >= length(ins) do
        {:term, acc, pos}
      else
        new_seen = MapSet.put(seen, pos)
        {new_pos, new_acc} = execute(Enum.at(ins, pos), pos, acc)
        run(ins, new_pos, new_acc, new_seen)
      end
    end
  end

  def part1(instructions) do
    {_, acc, _} = run(instructions, 0, 0, MapSet.new())
    acc
  end

  def part2(instructions) do
    part2(instructions, 0)
  end

  defp part2(ins, i) when i >= length(ins), do: :noop

  defp part2(ins, i) do
    case run(replaceIns(ins, i), 0, 0, MapSet.new()) do
      {:infinte_loop, _, _} ->
        part2(ins, i+1)
      {:term, acc, pos} -> acc
    end
  end

  defp replaceIns(ins, i) do
    new_ins =
      case Enum.at(ins, i) do
        {:nop, num} -> {:jmp, num}
        {:jmp, num} -> {:nop, num}
        {:acc, num} -> {:acc, num}
      end

    List.replace_at(ins, i, new_ins)
  end

  def parse_input(txt) do
    txt
    |> String.split("\n")
    |> Enum.map(&parse_instruction(&1))
  end

  defp parse_instruction("nop " <> val) do
    {num, ""} = Integer.parse(val)
    {:nop, num}
  end

  defp parse_instruction("acc " <> val) do
    {num, ""} = Integer.parse(val)
    {:acc, num}
  end

  defp parse_instruction("jmp " <> val) do
    {num, ""} = Integer.parse(val)
    {:jmp, num}
  end
end
