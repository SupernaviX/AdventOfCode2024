use "collections"
use "files"

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

class Robot
  var position: Point
  let velocity: (I64, I64)
  new create(position': Point, velocity': (I64, I64)) =>
    position = position'
    velocity = velocity'
  
  fun position_after(seconds: U64, bounds: (I64, I64)): Point =>
    let dx = velocity._1 * seconds.i64()
    let dy = velocity._2 * seconds.i64()
    let x = (position.x + dx) %% bounds._1
    let y = (position.y + dy) %% bounds._2
    Point(x, y)

class World
  let bounds: (I64, I64)
  let robots: Array[Robot]
  new create(bounds': (I64, I64), robots': Array[Robot]) =>
    bounds = bounds'
    robots = robots'

  fun positions_after(seconds: U64): Array[Point] =>
    let positions = Array[Point]
    for robot in robots.values() do
      positions.push(robot.position_after(seconds, bounds))
    end
    positions

actor Day14
  be part1(input: Array[String] val, out: OutStream) =>
    let world = parse_world(input)
    let positions = world.positions_after(100)
    (let tl, let tr, let bl, let br) = quadrantify(world.bounds, positions)
    let factor = ((tl * tr) * bl) * br
    out.print("part 1: " + factor.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let world = parse_world(input)
    var seconds: U64 = 0
    while true do
      let positions = world.positions_after(seconds)
      let cluster = biggest_cluster_size(world.bounds, positions)
      if cluster > (world.robots.size() / 3) then
        break
      end
      seconds = seconds + 1
    end
    out.print("part 2: " + seconds.string())
    out.print(string(world.bounds, world.positions_after(seconds)))

  fun parse_world(input: Array[String] val): World =>
    let bounds: (I64, I64) = (101, 103)
    let robots = parse_robots(input)
    World(bounds, robots)

  fun parse_robots(input: Array[String] val): Array[Robot] =>
    let robots = Array[Robot]
    for line in input.values() do
      try
        robots.push(parse_robot(line)?)
      end
    end
    robots

  fun parse_robot(line: String): Robot ? =>
    var offset: ISize = 2 // p=
    (let px, var offset') = line.read_int[I64](offset)?
    offset = offset + offset'.isize() + 1 // ,
    (let py, offset') = line.read_int[I64](offset)?
    offset = offset + offset'.isize() + 3 // <space>v=
    (let vx, offset') = line.read_int[I64](offset)?
    offset = offset + offset'.isize() + 1 // ,
    (let vy, offset') = line.read_int[I64](offset)?
    Robot(Point(px, py), (vx, vy))

  fun quadrantify(bounds: (I64, I64), positions: Array[Point]): (U64, U64, U64, U64) =>
    let mx = bounds._1 / 2
    let my = bounds._2 / 2
    var tl: U64 = 0
    var tr: U64 = 0
    var bl: U64 = 0
    var br: U64 = 0
    for pos in positions.values() do
      if (pos.x < mx) and (pos.y < my) then
        tl = tl + 1
      elseif (pos.x > mx) and (pos.y < my) then
        tr = tr + 1
      elseif (pos.x < mx) and (pos.y > my) then
        bl = bl + 1
      elseif (pos.x > mx) and (pos.y > my) then
        br = br + 1
      end
    end
    (tl, tr, bl, br)

  fun string(bounds: (I64, I64), positions: Array[Point]): String =>
    let rows = Array[String ref]
    for row in Range(0, bounds._2.usize()) do
      rows.push(" ".repeat_str(bounds._1.usize()))
    end
    for position in positions.values() do
      try
        rows(position.y.usize())?(position.x.usize())? = 'x'
      end
    end
    "\n".join(rows.values())

  fun biggest_cluster_size(bounds: (I64, I64), positions: Array[Point]): USize =>
    let dirs: Array[(I64, I64)] = [(-1, 0); (1, 0); (0, -1); (0, 1)]
    let available = Set[Point]
    for point in positions.values() do
      available.set(point)
    end
    var biggest: USize = 0
    for point in positions.values() do
      try
        available.extract(point)?
      else
        continue
      end
      let cluster = Set[Point]
      let frontier = Array[Point]
      frontier.push(point)
      while true do
        let curr = try frontier.pop()? else break end
        cluster.set(curr)
        for dir in dirs.values() do
          let next = Point(curr.x + dir._1, curr.y + dir._2)
          try
            available.extract(next)?
          else
            continue
          end
          if not cluster.contains(next) then
            frontier.push(next)
          end
        end
      end
      if cluster.size() > biggest then biggest = cluster.size() end
    end
    biggest

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

    Day14.part1(input', env.out)
    Day14.part2(input', env.out)