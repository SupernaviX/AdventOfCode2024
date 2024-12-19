use "collections"
use "files"

class TowelArranger
  let towels: Array[String]
  new create(towels': Array[String]) =>
    towels = towels'

  fun valid(pattern: String): Bool =>
    let frontier = List[USize]
    frontier.push(0)
    let seen = Set[USize]
    while frontier.size() != 0 do
      let next = try frontier.pop()? else break end
      if next == pattern.size() then return true end
      if seen.contains(next) then continue end
      seen.set(next)

      for towel in towels.values() do
        if pattern.at(towel, next.isize()) then
          frontier.push(next + towel.size())
        end
      end
    end
    false

  fun count_valid(pattern: String): U64 =>
    _count_valid(pattern, 0, Map[USize, U64])
  
  fun _count_valid(pattern: String, from: USize, cache: Map[USize, U64]): U64 =>
    if from == pattern.size() then
      return 1
    end
    try
      return cache(from)?
    end
    var sum: U64 = 0
    for towel in towels.values() do
      if pattern.at(towel, from.isize()) then
        sum = sum + _count_valid(pattern, from + towel.size(), cache)
      end
    end
    cache(from) = sum
    sum
  
actor Day19
  be part1(input: Array[String] val, out: OutStream) =>
    (let arranger, let patterns) = parse_towels(input)
    var satisfied: U64 = 0
    for pattern in patterns.values() do
      if arranger.valid(pattern) then
        satisfied = satisfied + 1
      end
    end
    out.print("part 1: " + satisfied.string())

  be part2(input: Array[String] val, out: OutStream) =>
    (let arranger, let patterns) = parse_towels(input)
    var satisfied: U64 = 0
    for pattern in patterns.values() do
      satisfied = satisfied + arranger.count_valid(pattern)
    end
    out.print("part 2: " + satisfied.string())

  fun parse_towels(input: Array[String] val): (TowelArranger, Array[String]) =>
    var towels = Array[String]
    let patterns = Array[String]
    for (index, line) in input.pairs() do
      if index == 0 then
        towels = line.split_by(", ")
      elseif index > 1 then
        patterns.push(line)
      end
    end
    (TowelArranger(towels), patterns)

actor Main
  new create(env: Env) =>
    let filename = try
      env.args(1)?
    else
      env.err.print("Invalid path")
      return
    end
    let input: Array[String] iso = Array[String]
    let path = FilePath(FileAuth(env.root), filename)
    match OpenFile(path)
      | let file: File =>
        for line in file.lines() do
          input.push(consume line)
        end
      else
        env.err.print("Could not read file")
        return
      end
    let input': Array[String] val = recover val
      consume input
    end

    Day19.part1(input', env.out)
    Day19.part2(input', env.out)