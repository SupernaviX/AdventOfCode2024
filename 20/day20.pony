use "collections"
use "files"
use "itertools"

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

class State is Comparable[State]
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
  let start: Point
  let finish: Point
  let walls: Set[Point]

  new create(bounds': (I64, I64), start': Point, finish': Point, walls': Set[Point]) =>
    bounds = bounds'
    start = start'
    finish = finish'
    walls = walls'

  fun distances_with_cheats(cheat_len: U64): Map[U64, U64] =>
    let dists_from_start = distances_from(start)
    let dists_from_finish = distances_from(finish)
    let distances = Map[U64, U64]
    for (from, from_dist) in dists_from_start.pairs() do
      for (to, cheat_dist) in all_cheat_dests(from, cheat_len).values() do
        let to_dist = try dists_from_finish(to)? else continue end
        let distance = from_dist + to_dist + cheat_dist
        let count = distances.get_or_else(distance, 0)
        distances(distance) = count + 1
      end
    end
    distances

  fun all_cheat_dests(from: Point, distance: U64): Array[(Point, U64)] =>
    let results = Array[(Point, U64)]
    for x in Range[I64](-distance.i64(), distance.i64() * 2) do
      for y in Range[I64](-distance.i64(), distance.i64() * 2) do
        let point = Point(from.x + x, from.y + y)
        let dist = x.abs() + y.abs()
        if ((dist > 1) and (dist <= distance)) and legal_move(point) then
          results.push((point, dist))
        end
      end
    end
    results

  fun distances_from(point: Point): Map[Point, U64] =>
    let seen = Map[Point, U64]
    let frontier = List[State]
    frontier.push(State(point, 0))
    while frontier.size() != 0 do
      let state = try frontier.pop()? else break end
      if seen.contains(state.point) then continue end
      seen(state.point) = state.distance_so_far
      for dir in [North; South; East; West].values() do
        let point' = state.point + dir
        if not legal_move(point') then
          continue
        end
        let state' = State(point', state.distance_so_far + 1)
        frontier.push(state')
      end
    end
    seen

  fun legal_move(point: Point): Bool =>
    in_bounds(point) and (not walls.contains(point))

  fun in_bounds(point: Point): Bool =>
    if point.x < 0 then return false end
    if point.x >= bounds._1 then return false end
    if point.y < 0 then return false end  
    if point.y >= bounds._2 then return false end
    true

actor Day20
  be part1(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    let solution = solve(grid, 2)
    out.print("part 1: " + solution.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    let solution = solve(grid, 20)
    out.print("part 2: " + solution.string())

  fun solve(grid: Grid, cheat_len: U64): U64 =>
    let distances = grid.distances_from(grid.start)
    let honest_dist = try distances(grid.finish)? else return 0 end
    let max_dist = honest_dist - 100
    var total_acceptable_routes: U64 = 0
    for (distance, cheats) in grid.distances_with_cheats(cheat_len).pairs() do
      if distance > max_dist then continue end
      total_acceptable_routes = total_acceptable_routes + cheats
    end
    total_acceptable_routes

  fun parse_grid(input: Array[String] val): Grid =>
    let bounds = (try input(0)?.size().i64() else 0 end, input.size().i64())
    var start = Point(0, 0)
    var finish = Point(0, 0)
    let walls = Set[Point]
    for (y, row) in input.pairs() do
      for (x, cell) in Iter[U8](row.values()).collect(Array[U8]).pairs() do
        let point = Point(x.i64(), y.i64())
        if cell == 'S' then
          start = point
        elseif cell == 'E' then
          finish = point
        elseif cell == '#' then
          walls.set(point)
        end
      end
    end
    Grid(bounds, start, finish, walls)

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

    Day20.part1(input', env.out)
    Day20.part2(input', env.out)