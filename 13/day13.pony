use "files"

class Point
  let x: U64
  let y: U64
  new create(x': U64, y': U64) =>
    x = x'
    y = y'

  fun add(that: box->Point): Point =>
    Point(x + that.x, y + that.y)

  fun mul(amount: U64): Point =>
    Point(x * amount, y * amount)

  fun eq(that: box->Point): Bool =>
    (x == that.x) and (y == that.y)

  fun steps_to(that: box->Point, delta: box->Point): U64 ? =>
    let x_dist = that.x - x
    if (x_dist % delta.x) != 0 then error end
    let y_dist = that.y - y
    if (y_dist % delta.y) != 0 then error end
    let x_steps = x_dist / delta.x
    let y_steps = y_dist / delta.y
    if x_steps == y_steps then x_steps else error end

class Machine
  let a: Point
  let b: Point
  let prize: Point
  new create(a': Point, b': Point, prize': Point) =>
    a = a'
    b = b'
    prize = prize'

  fun presses(): (U64, U64) ? =>
    let ax = a.x.f64()
    let ay = a.y.f64()
    let bx = b.x.f64()
    let by = b.y.f64()
    let px = prize.x.f64()
    let py = prize.y.f64()
    // A * ax + B * bx = px
    // A * ay + B * by = py

    // (ax / ay) * (A * ay + B * by) = (ax / ay) * py
    // (A * ay * ax / ay) + (B * by * ax / ay) = (py * ax / ay)
    // (A * ax)           + (B * by * ax / ay) = (py * ax / ay)

    // (A * ax) + (B * bx) - (A * ax) - (B * by * ax / ay) = px - (py * ax / ay)
    // (B * bx) - (B * by * ax / ay) = px - (py * ax / ay)
    // B * (bx - (by * ax / ay)) = px - (py * ax / ay)
    // B = (px - (py * ax / ay)) / (bx - (by * ax / ay))
    let b_presses = (px - ((py * ax) / ay)) / (bx - ((by * ax) / ay))
    // A = (px - B * bx) / ax
    let a_presses = (px - (b_presses * bx)) / ax

    let a' = a_presses.round().u64()
    let b' = b_presses.round().u64()
    if ((a * a') + (b * b')) == prize then
      (a', b')
    else
      error
    end

  fun min_cost(): U64 ? =>
    (let a_presses, let b_presses) = presses()?
    (a_presses * 3) + b_presses

actor Day13
  be part1(input: Array[String] val, out: OutStream) =>
    let machines = parse_machines(input)
    var sum: U64 = 0
    for machine in machines.values() do
      sum = sum + try machine.min_cost()? else 0 end
    end
    out.print("part 1: " + sum.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let machines = parse_machines(input)
    var sum: U64 = 0
    for machine in machines.values() do
      let machine' = Machine(machine.a, machine.b, machine.prize + Point(10000000000000, 10000000000000))
      sum = sum + try machine'.min_cost()? else 0 end
    end
    out.print("part 2: " + sum.string())

  fun parse_machines(input: Array[String] val): Array[Machine] =>
    let lines = input.reverse()
    let result = Array[Machine]
    try
      while true do
        let a = parse_point(lines.pop()?, "Button A")?
        let b = parse_point(lines.pop()?, "Button B")?
        let prize = parse_point(lines.pop()?, "Prize")?
        result.push(Machine(a, b, prize))
        lines.pop()?
      end
    end
    result

  fun parse_point(line: String, label: String): Point ? =>
    let parts = line.substring(label.size().isize() + 2).split_by(", ")
    let x: U64 = parts(0)?.substring(2).u64()?
    let y: U64 = parts(1)?.substring(2).u64()?
    Point(x, y)

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

    Day13.part1(input', env.out)
    Day13.part2(input', env.out)