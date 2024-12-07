use "collections"
use "files"
use "itertools"

primitive Loop
primitive Leave

interface Direction is Hashable
  fun delta(): (I64, I64)
  fun right(): Direction val

primitive North is Direction
  fun delta(): (I64, I64) => (0, -1)
  fun right(): Direction val => East
  fun hash(): USize => 0
primitive South is Direction
  fun delta(): (I64, I64) => (0,  1)
  fun right(): Direction val => West
  fun hash(): USize => 1
primitive East is Direction
  fun delta(): (I64, I64) => ( 1, 0)
  fun right(): Direction val => South
  fun hash(): USize => 2
primitive West is Direction
  fun delta(): (I64, I64) => (-1, 0)
  fun right(): Direction val => North
  fun hash(): USize => 3

class Point is (Hashable & Equatable[Point ref] & Stringable)
  let x: I64
  let y: I64
  new create(x': I64, y': I64) =>
    x = x'
    y = y'

  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + x.hash()
    h = (31 * h) + y.hash()
    h
  
  fun eq(that: box->Point): Bool =>
    (x == that.x) and (y == that.y)

  fun string(): String iso^ => "(" + x.string() + ", " + y.string() + ")"

  fun clone(): Point => Point(x, y)

  fun add(dir: Direction val): Point =>
    (let dx, let dy) = dir.delta()
    Point(x + dx, y + dy)

  fun in_bounds(bounds: (USize, USize)): Bool =>
    (x >= 0) and (y >= 0) and (x.usize() < bounds._1) and (y.usize() < bounds._2)

class Guard is (Hashable & Equatable[Guard ref])
  let position: Point
  let direction: Direction val

  new create(position': Point, direction': Direction val) =>
    position = position'
    direction = direction'

  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + position.hash()
    h = (31 * h) + direction.hash()
    h
  
  fun eq(that: box->Guard): Bool =>
    (position == that.position) and (direction is that.direction)

  fun clone(): Guard =>
    Guard(position.clone(), direction)

  fun step(): Guard =>
    Guard(position + direction, direction)

  fun turn(): Guard =>
    Guard(position.clone(), direction.right())

class Grid
  let size: (USize, USize)
  var obstacles: Set[Point]
  let initial_guard: Guard
  var guard: Guard
  var visited: Set[Point]
  var path: Set[Guard]
  new create(size': (USize, USize), obstacles': Set[Point], guard': Guard) =>
    size = size'
    obstacles = obstacles'
    initial_guard = guard'
    guard = guard'
    visited = Set[Point]
    visited.set(guard.position)
    path = Set[Guard]

  fun ref step(): (Loop | Leave | None) =>
    let pos = guard.position + guard.direction
    if not pos.in_bounds(size) then
      return Leave
    elseif obstacles.contains(pos) then
      guard = guard.turn()
    else
      guard = guard.step()
      visited.set(guard.position)
    end
    if path.contains(guard) then
      Loop
    else
      path.set(guard)
      None
    end

  fun ref solve(): (Loop | Leave) =>
    while true do
      match step()
      | Loop => return Loop
      | Leave => return Leave
      | None => continue
      end
    end
    Loop

  fun with_obstacle(pos: Point): Grid =>
    var obstacles': Set[Point] = Set[Point]
    for obs in obstacles.values() do
      obstacles'.set(obs.clone())
    end
    obstacles'.set(pos)
    Grid(size, obstacles', initial_guard.clone())

actor Day6
  be part1(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    grid.solve()
    out.print("part 1: " + grid.visited.size().string())

  be part2(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    let guard_start = grid.guard.position
    grid.solve()
    let candidates = grid.visited
    candidates.unset(guard_start)

    var total: U64 = 0
    for candidate in candidates.values() do
      let grid' = grid.with_obstacle(candidate)
      if grid'.solve() is Loop then
        total = total + 1
      end
    end

    out.print("part 2: " + total.string())

  fun parse_grid(input: Array[String] val): Grid =>
    let size = (try input(0)?.size() else 0 end, input.size())
    var obstacles: Set[Point] = Set[Point]
    var guard_pos = Point(0, 0)
    for (y, row) in input.pairs() do
      for (x, cell) in Iter[U8](row.values()).collect(Array[U8]).pairs() do
        match cell
        | '#' => obstacles.set(Point(x.i64(), y.i64()))
        | '^' => guard_pos = Point(x.i64(), y.i64())
        else None
        end
        None
      end
      None
    end
    Grid(size, obstacles, Guard(guard_pos, North))

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

    Day6.part1(input', env.out)
    Day6.part2(input', env.out)