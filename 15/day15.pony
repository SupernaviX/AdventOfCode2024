use "collections"
use "files"
use "itertools"

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

  fun add(dir: Direction val): Point =>
    let delta = dir.delta()
    Point(x + delta._1, y + delta._2)

  fun wide_neighbors(dir: Direction val): Array[WidePoint] =>
    match dir.delta()
      | (-1,  0) => [WidePoint(x - 2, y)]
      | ( 1,  0) => [WidePoint(x + 1, y)]
      | ( 0, -1) => [WidePoint(x - 1, y - 1); WidePoint(x, y - 1)]
      | ( 0,  1) => [WidePoint(x - 1, y + 1); WidePoint(x, y + 1)]
      else Array[WidePoint]
    end

class Grid
  let walls: Set[Point]
  let boxes: Set[Point]
  var robot: Point

  new create(walls': Set[Point], boxes': Set[Point], robot': Point) =>
    walls = walls'
    boxes = boxes'
    robot = robot'
  
  fun ref move(dir: Direction val) =>
    let next = robot + dir
    var last = next
    while boxes.contains(last) do
      last = last + dir
    end
    if walls.contains(last) then return end

    if next != last then
      boxes.unset(next)
      boxes.set(last)
    end
    robot = next

  fun gps(): U64 =>
    var sum: U64 = 0
    for point in boxes.values() do
      sum = sum + (point.x.u64() + (100 * point.y.u64()))
    end
    sum

class WidePoint is (Hashable & Equatable[WidePoint ref])
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
  
  fun eq(that: box->WidePoint): Bool =>
    (x == that.x) and (y == that.y)

  fun add(dir: Direction val): WidePoint =>
    let delta = dir.delta()
    WidePoint(x + delta._1, y + delta._2)

  fun neighbors(dir: Direction val): Array[Point] =>
    match dir.delta()
      | (-1,  0) => [Point(x - 1, y)]
      | ( 1,  0) => [Point(x + 2, y)]
      | ( 0, -1) => [Point(x, y - 1); Point(x + 1, y - 1)]
      | ( 0,  1) => [Point(x, y + 1); Point(x + 1, y + 1)]
      else Array[Point]
    end

  fun wide_neighbors(dir: Direction val): Array[WidePoint] =>
    match dir.delta()
      | (-1,  0) => [WidePoint(x - 2, y)]
      | ( 1,  0) => [WidePoint(x + 2, y)]
      | ( 0, -1) => [WidePoint(x - 1, y - 1); WidePoint(x, y - 1); WidePoint(x + 1, y - 1)]
      | ( 0,  1) => [WidePoint(x - 1, y + 1); WidePoint(x, y + 1); WidePoint(x + 1, y + 1)]
      else Array[WidePoint]
    end

class BigGrid
  let walls: Set[Point]
  let boxes: Set[WidePoint]
  var robot: Point
  new create(grid: Grid) =>
    walls = Set[Point]
    for wall in grid.walls.values() do
      walls.set(Point(wall.x * 2, wall.y))
      walls.set(Point((wall.x * 2) + 1, wall.y))
    end
    boxes = Set[WidePoint]
    for point in grid.boxes.values() do
      boxes.set(WidePoint(point.x * 2, point.y))
    end
    robot = Point(grid.robot.x * 2, grid.robot.y)

  fun ref move(dir: Direction val) =>
    let next = robot + dir
    if walls.contains(next) then return end

    // Find all the boxes which the robot will move
    let all_moving = Set[WidePoint]
    var frontier = Iter[WidePoint](robot.wide_neighbors(dir).values())
      .filter({(w) => boxes.contains(w)})
      .collect(Array[WidePoint])
    while frontier.size() != 0 do
      let new_frontier = Array[WidePoint]
      for moving in frontier.values() do
        for neighbor in moving.neighbors(dir).values() do
          if walls.contains(neighbor) then
            // the way is blocked
            return
          end
        end
        for neighbor in moving.wide_neighbors(dir).values() do
          if boxes.contains(neighbor) then
            // this block is pushing another block
            new_frontier.push(neighbor) 
          end
        end
        all_moving.set(moving)
      end
      frontier = new_frontier
    end

    // actually move everything
    for moving in all_moving.values() do
      boxes.unset(moving)
    end
    for moving in all_moving.values() do
      boxes.set(moving + dir)
    end
    robot = next

  fun gps(): U64 =>
    var sum: U64 = 0
    for point in boxes.values() do
      sum = sum + (point.x.u64() + (100 * point.y.u64()))
    end
    sum

actor Day15
  be part1(input: Array[String] val, out: OutStream) =>
    (let grid, let dirs) = parse_input(input)
    for dir in dirs.values() do
      grid.move(dir)
    end
    out.print("part 1: " + grid.gps().string())

  be part2(input: Array[String] val, out: OutStream) =>
    (let grid, let dirs) = parse_input(input)
    let big_grid = BigGrid(grid)
    for dir in dirs.values() do
      big_grid.move(dir)
    end
    out.print("part 2: " + big_grid.gps().string())
    None

  fun parse_input(input: Array[String] val): (Grid, Array[Direction val]) =>
    let walls = Set[Point]
    let boxes = Set[Point]
    var robot = Point(-1, -1)

    let lines = input.reverse()
    var y: I64 = 0
    while true do
      let line = try lines.pop()? else break end
      if line.size() == 0 then break end
      for (x, char) in Iter[U8](line.values()).collect(Array[U8]).pairs() do
        let point = Point(x.i64(), y.i64())
        if char == '#' then
          walls.set(point)
        elseif char == 'O' then
          boxes.set(point)
        elseif char == '@' then
          robot = point
        end
      end
      y = y + 1
    end

    let grid = Grid(walls, boxes, robot)
    let moves = Array[Direction val]
    try
      while true do
        for char in lines.pop()?.values() do
          if char == '^' then
            moves.push(North)
          elseif char == 'v' then
            moves.push(South)
          elseif char == '>' then
            moves.push(East)
          elseif char == '<' then
            moves.push(West)
          end
        end
      end
    end
    (grid, moves)

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

    Day15.part1(input', env.out)
    Day15.part2(input', env.out)