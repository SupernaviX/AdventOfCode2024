use "collections"
use "files"
use "itertools"

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
  
  fun string(): String iso^ => "(" + x.string() + ", " + y.string() + ")"

  fun clone(): Point => Point(x, y)

  fun add(that: box->Point): Point =>
    Point(x + that.x, y + that.y)
  
  fun sub(that: box->Point): Point =>
    Point(x - that.x, y - that.y)

  fun in_bounds(bounds: (USize, USize)): Bool =>
    (x >= 0) and (y >= 0) and (x.usize() < bounds._1) and (y.usize() < bounds._2)

class Grid
  let size: (USize, USize)
  let antennas: Map[U8, Array[Point]]
  new create(size': (USize, USize), antennas': Map[U8, Array[Point]]) =>
    size = size'
    antennas = antennas'

  fun antinodes(): Set[Point] =>
    let all = Set[Point]
    for points in antennas.values() do
      for antinode in antinodes_from_points(points).values() do
        all.set(antinode)
      end
    end
    all
  
  fun antinodes_from_points(points: Array[Point] box): Set[Point] =>
    let all = Set[Point]
    for p1 in points.values() do
      for p2 in points.values() do
        if p1 == p2 then continue end
        let difference = p2 - p1
        let anti1 = p2 + difference
        if anti1.in_bounds(size) then
          all.set(anti1)
        end
        let anti2 = p1 - difference
        if anti2.in_bounds(size) then
          all.set(anti2)
        end
      end
    end
    all

  fun resonant_antinodes(): Set[Point] =>
    let all = Set[Point]
    for points in antennas.values() do
      for antinode in resonant_antinodes_from_points(points).values() do
        all.set(antinode)
      end
    end
    all

  fun resonant_antinodes_from_points(points: Array[Point] box): Set[Point] =>
    let all = Set[Point]
    for p1 in points.values() do
      for p2 in points.values() do
        if p1 == p2 then continue end
        let difference = p2 - p1

        var point = p2
        repeat
          all.set(point.clone())
          point = point + difference
        until not point.in_bounds(size) end

        point = p1
        repeat
          all.set(point.clone())
          point = point - difference
        until not point.in_bounds(size) end
      end
    end
    all

actor Day8
  be part1(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    out.print("part 1: " + grid.antinodes().size().string())
    None

  be part2(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    out.print("part 2: " + grid.resonant_antinodes().size().string())
    None

  fun parse_grid(input: Array[String] val): Grid =>
    let size = (try input(0)?.size() else 0 end, input.size())
    let antennas = Map[U8, Array[Point]]
    for (y, row) in input.pairs() do
      for (x, cell) in Iter[U8](row.values()).collect(Array[U8]).pairs() do
        if cell != '.' then
          antennas.insert_if_absent(cell, Array[Point]).push(Point(x.i64(), y.i64()))
        end
      end
    end
    Grid(size, antennas)

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

    Day8.part1(input', env.out)
    Day8.part2(input', env.out)