use "collections"
use "files"

interface Direction
  fun delta(): (I64, I64)

primitive North is Direction
  fun delta(): (I64, I64) => (0, -1)
primitive South is Direction
  fun delta(): (I64, I64) => (0,  1)
primitive East is Direction
  fun delta(): (I64, I64) => ( 1, 0)
primitive West is Direction
  fun delta(): (I64, I64) => (-1, 0)

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
  
class Grid
  let tiles: Array[Array[U8]]
  let size: (USize, USize)
  new create(tiles': Array[Array[U8]]) =>
    tiles = tiles'
    size = (try tiles(0)?.size() else 0 end, tiles.size())
  
  fun score_trailheads(): U64 =>
    var score: U64 = 0
    for (y, row) in tiles.pairs() do
      for (x, cell) in row.pairs() do
        if cell != 0 then continue end
        score = score + score_trailhead(Point(x.i64(), y.i64()))
      end
    end
    score

  fun score_trailhead(at: Point): U64 =>
    let dirs: Array[Direction val] = [North; South; East; West]
    var next = Set[Point]
    next.set(at)
    for seek in Range[U8](1, 10) do
      let next' = Set[Point]
      for from' in next.values() do
        for dir in dirs.values() do
          let to' = from'.move(dir)
          if try value_at(to')? == seek else false end then
            next'.set(to')
          end
        end
      end
      next = next'
      if next.size() == 0 then
        return 0
      end
    end
    next.size().u64()

  fun rate_trailheads(): U64 =>
    var score: U64 = 0
    for (y, row) in tiles.pairs() do
      for (x, cell) in row.pairs() do
        if cell != 0 then continue end
        score = score + rate_point(Point(x.i64(), y.i64()), Map[Point, U64])
      end
    end
    score


  fun rate_point(at: Point, cache: Map[Point, U64]): U64 =>
    let dirs: Array[Direction val] = [North; South; East; West]
    try
      return cache(at)?
    end
    let value = try value_at(at)? else return 0 end
    if value == 9 then return 1 end
    var score: U64 = 0
    for dir in dirs.values() do
      let at' = at.move(dir)
      if try value_at(at')? == (value + 1) else false end then
        score = score + rate_point(at', cache)
      end
    end
    cache(at) = score
    score

  fun value_at(point: Point): U8 ? =>
    tiles(point.y.usize())?(point.x.usize())?

actor Day10
  be part1(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    out.print("part 1: " + grid.score_trailheads().string())
    None

  be part2(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    out.print("part 2: " + grid.rate_trailheads().string())
    None

  fun parse_grid(input: Array[String] val): Grid =>
    let tiles = Array[Array[U8]]
    for row in input.values() do
      let tile_row = Array[U8]
      for cell in row.values() do
        tile_row.push(cell - '0')
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

    Day10.part1(input', env.out)
    Day10.part2(input', env.out)
