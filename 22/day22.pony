use "collections"
use "files"
use "itertools"

class Prng is Iterator[U64]
  var value: U64
  new create(seed: U64) =>
    value = seed

  fun ref has_next(): Bool => true
  fun ref next(): U64 =>
    value = (value xor (value << 6)) and 0xffffff
    value = (value xor (value >> 5)) and 0xffffff
    value = (value xor (value << 11)) and 0xffffff
    value

class val Delta is (Hashable & Equatable[Delta] & Stringable) 
  let changes: Array[I64]
  new val create(c0: I64, c1: I64, c2: I64, c3: I64) =>
    changes = [c0; c1; c2; c3]

  fun hash(): USize =>
    var h: USize = 7
    for change in changes.values() do
      h = (31 * h) + change.hash()
    end
    h
  
  fun eq(that: box->Delta): Bool =>
    if changes.size() != that.changes.size() then return false end
    for (i, change) in changes.pairs() do
      let change' = try that.changes(i)? else return false end
      if change != change' then return false end
    end
    true

  fun string(): String iso^ =>
    ",".join(changes.values())

class Monkey
  let seed: U64
  new create(seed': U64) =>
    seed = seed'

  fun price_at(at: USize): U64 =>
    try
      Iter[U64](Prng(seed)).nth(at)?
    else
      0
    end

  fun deltas(up_to: USize): Map[Delta, U64] =>
    let latest_changes = Array[I64]
    let prices = Map[Delta, U64]
    var old_price = (seed % 10).i64()
    for price in Iter[U64](Prng(seed)).map[I64]({(p) => (p % 10).i64()}).take(up_to) do
      latest_changes.push(price - old_price)
      if latest_changes.size() > 4 then
        try
          latest_changes.shift()?
          let delta = Delta(latest_changes(0)?, latest_changes(1)?, latest_changes(2)?, latest_changes(3)?)
          prices.insert_if_absent(delta, price.u64())
        end
      end
      old_price = price
    end
    prices

actor Day22
  be part1(input: Array[String] val, out: OutStream) =>
    let monkeys = parse_input(input)
    var total: U64 = 0
    for monkey in monkeys.values() do
      total = total + monkey.price_at(2000)
    end
    out.print("part 1: " + total.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let monkeys = parse_input(input)
    let delta_prices = Map[Delta, U64]
    for (i, monkey) in monkeys.pairs() do
      let deltas = monkey.deltas(2000)
      for (delta, price) in deltas.pairs() do
        let old_price = delta_prices.get_or_else(delta, 0)
        delta_prices(delta) = old_price + price
      end
    end
    var max_price = U64(0)
    for (delta, price) in delta_prices.pairs() do
      if price > max_price then
        max_price = price
      end
    end
    out.print("part 2: " + max_price.string())
  
  fun parse_input(input: Array[String] val): Array[Monkey] =>
    let result = Array[Monkey]
    for line in input.values() do
      let seed = try line.u64()? else continue end
      result.push(Monkey(seed))
    end
    result

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

    Day22.part1(input', env.out)
    Day22.part2(input', env.out)