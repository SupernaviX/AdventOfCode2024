use "collections"
use "files"

interface Direction is (Hashable & Equatable[Direction])
  fun delta(): (I64, I64)

primitive North is Direction
  fun delta(): (I64, I64) => (0, -1)
  fun hash(): USize => 0
primitive South is Direction
  fun delta(): (I64, I64) => (0,  1)
  fun hash(): USize => 1
primitive East is Direction
  fun delta(): (I64, I64) => ( 1, 0)
  fun hash(): USize => 2
primitive West is Direction
  fun delta(): (I64, I64) => (-1, 0)
  fun hash(): USize => 3

class Side is (Hashable & Equatable[Side ref])
  let coord: I64
  let dir: Direction val
  new create(coord': I64, dir': Direction val) =>
    coord = coord'
    dir = dir'

  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + coord.hash()
    h = (31 * h) + dir.hash()
    h

  fun eq(that: box->Side): Bool =>
    (coord == that.coord) and (dir == that.dir)

class Point is (Hashable & Equatable[Point ref])
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

  fun move(dir: Direction val): Point =>
    (let x', let y') = dir.delta()
    Point(x + x', y + y')
    
class Plot
  let points: Set[Point]
  new create(points': Set[Point]) =>
    points = points'

  fun area(): U64 =>
    points.size().u64()

  fun perimeter(): U64 =>
    let dirs: Array[Direction val] = [North; South; East; West]
    var sum: U64 = 0
    for point in points.values() do
      for dir in dirs.values() do
        let point' = point.move(dir)
        if not points.contains(point') then
          sum = sum + 1
        end
      end
    end
    sum

  fun discount_perimeter(): U64 =>
    let sides = Map[Side, Array[I64]]
    for point in points.values() do
      if not points.contains(point.move(North)) then
        sides.insert_if_absent(Side(point.y, North), Array[I64]).push(point.x)
      end
      if not points.contains(point.move(South)) then
        sides.insert_if_absent(Side(point.y, South), Array[I64]).push(point.x)
      end
      if not points.contains(point.move(East)) then
        sides.insert_if_absent(Side(point.x, East), Array[I64]).push(point.y)
      end
      if not points.contains(point.move(West)) then
        sides.insert_if_absent(Side(point.x, West), Array[I64]).push(point.y)
      end
    end
    var sum: U64 = 0
    for side_points in sides.values() do
      let sorted_points = Sort[Array[I64], I64](side_points)
      for (i, point) in sorted_points.pairs() do
        if try sorted_points(i - 1)? != (point - 1) else true end then
          sum = sum + 1
        end
      end
    end
    sum

class Grid
  let tiles: Array[Array[U8]]
  let size: (USize, USize)
  new create(tiles': Array[Array[U8]]) =>
    tiles = tiles'
    size = (try tiles(0)?.size() else 0 end, tiles.size())
  
  fun find_plots(): Array[Plot] =>
    let plots = Array[Plot]
    let taken = Set[Point]
    for (y, row) in tiles.pairs() do
      for (x, tile) in row.pairs() do
        let point = Point(x.i64(), y.i64())
        if taken.contains(point) then
          continue
        end
        plots.push(gather_plot(point, taken))
      end
    end
    plots

  fun gather_plot(start: Point, taken: Set[Point]): Plot =>
    let dirs: Array[Direction val] = [North; South; East; West]
    let points = Set[Point]
    let seeking = try find_tile(start)? else return Plot(points) end
    let frontier = Array[Point]
    frontier.push(start)
    while frontier.size() > 0 do
      let next = try frontier.pop()? else break end
      points.set(next)
      taken.set(next)

      for dir in dirs.values() do
        let point' = next.move(dir)
        if taken.contains(point') then continue end
        if try find_tile(point')? == seeking else false end then
          frontier.push(point')
        end
      end
    end
    Plot(points)

  fun find_tile(point: Point): U8 ? =>
    tiles(point.y.usize())?(point.x.usize())?

actor Day12
  be part1(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    let plots = grid.find_plots()
    var price: U64 = 0
    for (tile, plot) in plots.pairs() do
      price = price + (plot.area() * plot.perimeter())
    end
    out.print("part 1: " + price.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    let plots = grid.find_plots()
    var price: U64 = 0
    for (tile, plot) in plots.pairs() do
      price = price + (plot.area() * plot.discount_perimeter())
    end
    out.print("part 2: " + price.string())

  fun parse_grid(input: Array[String] val): Grid =>
    let tiles = Array[Array[U8]]
    for row in input.values() do
      let tile_row = Array[U8]
      for cell in row.values() do
        tile_row.push(cell)
      end
      tiles.push(tile_row)
    end
    Grid(tiles)

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

    Day12.part1(input', env.out)
    Day12.part2(input', env.out)