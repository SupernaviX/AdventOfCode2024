use "collections"
use "files"

class RockSteps is (Hashable & Equatable[RockSteps])
  let rock: U64
  let steps: USize
  new create(rock': U64, steps': USize) =>
    rock = rock'
    steps = steps'

  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + rock.hash()
    h = (31 * h) + steps.hash()
    h

  fun eq(that: box->RockSteps): Bool =>
    (rock == that.rock) and (steps == that.steps)  

actor Day11
  be part1(input: String val, out: OutStream) =>
    var rocks = try parse_rocks(input)? else return end
    out.print("part 1: " + count_rocks_after(rocks, 25).string())
  
  be part2(input: String val, out: OutStream) =>
    var rocks = try parse_rocks(input)? else return end
    out.print("part 2: " + count_rocks_after(rocks, 75).string())

  fun parse_rocks(input: String val): Array[U64] ? =>
    let rocks = Array[U64]
    for rock in input.split(" ").values() do
      rocks.push(rock.u64()?)
    end
    rocks

  fun count_rocks_after(rocks: Array[U64], steps: USize): USize =>
    let cache = Map[RockSteps, USize]
    var sum: USize = 0
    for rock in rocks.values() do
      sum = sum + count_rock_after(rock, steps, cache)
    end
    sum

  fun count_rock_after(rock: U64, steps: USize, cache: Map[RockSteps, USize]): USize =>
    if steps == 0 then
      return 1
    end
    let rock_steps = RockSteps(rock, steps)
    try
      return cache(rock_steps)?
    end
    var sum: USize = 0
    for next_rock in update_rock(rock).values() do
      sum = sum + count_rock_after(next_rock, steps - 1, cache)
    end
    cache(rock_steps) = sum
    sum
  
  fun update_rock(rock: U64): Array[U64] =>
    if rock == 0 then
      return [1]
    end
    let rock_str = rock.string()
    if (rock_str.size() % 2) == 0 then
      (let left, let right) = rock_str.clone().chop(rock_str.size() / 2)
      try
        [left.u64()?; right.u64()?]
      else
        []
      end
    else
      [rock * 2024]
    end

actor Main
  new create(env: Env) =>
    let filename = try
      env.args(1)?
    else
      env.err.print("Invalid path")
      return
    end
    let path = FilePath(FileAuth(env.root), filename)
    let input = match OpenFile(path)
      | let file: File => file.read_string(file.size())
      else
        env.err.print("Could not read file")
        return
      end
    let input': String val = recover val
      consume input
    end
    Day11.part1(input', env.out)
    Day11.part2(input', env.out)