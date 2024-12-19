use "collections"
use "files"

interface val Direction is (Hashable & Stringable)
  fun delta(): (I64, I64)
  fun left(): Direction
  fun right(): Direction

primitive North is Direction
  fun delta(): (I64, I64) => (0, -1)
  fun left(): Direction => West
  fun right(): Direction => East
  fun hash(): USize => 0
  fun string(): String iso^ => "North".clone()
primitive South is Direction
  fun delta(): (I64, I64) => (0,  1)
  fun left(): Direction => East
  fun right(): Direction => West
  fun hash(): USize => 1
  fun string(): String iso^ => "South".clone()
primitive East is Direction
  fun delta(): (I64, I64) => ( 1, 0)
  fun left(): Direction => North
  fun right(): Direction => South
  fun hash(): USize => 2
  fun string(): String iso^ => "East".clone()
primitive West is Direction
  fun delta(): (I64, I64) => (-1, 0)
  fun left(): Direction => South
  fun right(): Direction => North
  fun hash(): USize => 3
  fun string(): String iso^ => "West".clone()
  
class val Point is (Hashable & Equatable[Point] & Stringable & Comparable[Point])
  let x: I64
  let y: I64
  new val create(x': I64, y': I64) =>
    x = x'
    y = y'

  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + x.hash()
    h = (31 * h) + y.hash()
    h

  fun lt(that: box->Point): Bool =>
    (x < that.x) or ((x == that.x) and (y < that.y))
  
  fun eq(that: box->Point): Bool =>
    (x == that.x) and (y == that.y)

  fun string(): String iso^ => "(" + x.string() + ", " + y.string() + ")"

  fun add(dir: Direction): Point =>
    (let dx, let dy) = dir.delta()
    Point(x + dx, y + dy)

class val State is Comparable[State]
  let point: Point
  let distance_so_far: U64
  new create(point': Point, distance_so_far': U64) =>
    point = point'
    distance_so_far = distance_so_far'

  fun eq(that: box->State): Bool =>
    ((distance_so_far == that.distance_so_far) and (point == that.point))

  fun lt(that: box->State): Bool =>
    ((distance_so_far < that.distance_so_far) or (point < that.point))

class Grid
  let bounds: (I64, I64)
  let walls: Map[Point, U64]
  new create(bounds': (I64, I64), walls': Map[Point, U64]) =>
    bounds = bounds'
    walls = walls'

  fun is_navigable(time: U64): Bool =>
    try
      navigate(time)?
      true
    else
      false
    end

  fun navigate(time: U64): U64 ? =>
    let goal = Point((bounds._1 - 1).i64(), (bounds._2 - 1).i64())
    let frontier = Array[State]
    frontier.push(State(Point(0, 0), 0))
    let seen = Set[Point]
    while frontier.size() != 0 do
      let next = frontier.shift()?
      if next.point == goal then return next.distance_so_far end
      if seen.contains(next.point) then continue end
      seen.set(next.point)

      for dir in [North; South; East; West].values() do
        let point' = next.point + dir
        if is_blocked(point', time) then continue end
        frontier.push(State(point', next.distance_so_far + 1))
      end
    end
    error
  
  fun is_blocked(point: Point, time: U64): Bool =>
    if (point.x < 0) or (point.x >= bounds._1) then
      true
    elseif (point.y < 0) or (point.y >= bounds._2) then
      true
    else
      try
        walls(point)? <= time
      else
        false
      end
    end

actor Day18
  be part1(input: Array[String] val, out: OutStream) =>
    let grid = parse(input)
    try
      let length = grid.navigate(1024)?
      out.print("part 1: " + length.string())
    else
      out.print("part 1 unsolvable")
    end
    None

  be part2(input: Array[String] val, out: OutStream) =>
    let grid = parse(input)
    var min_time: U64 = 0
    var max_time = input.size().u64()
    while true do
      let mid_time = (min_time + max_time) / 2
      if not grid.is_navigable(mid_time) then
        max_time = mid_time
        continue
      end
      if grid.is_navigable(mid_time + 1) then
        min_time = mid_time + 1
        continue
      end
      // Input element n falls at t=n+1.
      // `mid_time + 1` is the first time where the grid is not traversable,
      // so input element `mid_time` is the element which made it so.
      let point = try input(mid_time.usize())? else "n/a" end
      out.print("part 2: " + point)
      return
    end
    out.print("part 2 unsolvable")

  fun parse(input: Array[String] val): Grid =>
    let bounds: (I64, I64) = (71, 71)
    let walls = Map[Point, U64]
    for (time, line) in input.pairs() do
      try
        let point = parse_point(line)?
        walls(point) = time.u64() + 1
      end
    end
    Grid(bounds, walls)

  fun parse_point(line: String): Point ? =>
    let nums = line.split(",")
    let x = nums(0)?.i64()?
    let y = nums(1)?.i64()?
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

    Day18.part1(input', env.out)
    Day18.part2(input', env.out)