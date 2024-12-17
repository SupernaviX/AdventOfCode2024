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

class State is (Hashable & Equatable[State ref])
  let pos: Point
  let dir: Direction
  new create(pos': Point, dir': Direction) =>
    pos = pos'
    dir = dir'
  
  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + pos.hash()
    h = (31 * h) + dir.hash()
    h

  fun eq(that: box->State): Bool =>
    (pos == that.pos) and (dir is that.dir)

class Next is Comparable[Next ref]
  let state: State
  let score: U64
  new create(state': State, score': U64) =>
    state = state'
    score = score'

  fun lt(that: box->Next): Bool =>
    score < that.score

  fun eq(that: box->Next): Bool =>
    score == that.score

class Grid
  let start: Point
  let goal: Point
  let walls: Set[Point]
  new create(start': Point, goal': Point, walls': Set[Point]) =>
    start = start'
    goal = goal'
    walls = walls'

  fun navigate(): U64 =>
    let scores = Map[State, U64]
    let frontier = MinHeap[Next](0)
    frontier.push(Next(State(start, East), 0))
    while frontier.size() != 0 do
      let at = try frontier.pop()? else break end
      if scores.contains(at.state) then continue end
      if at.state.pos == goal then
        return at.score
      end
      scores(at.state) = at.score
      let next_pos = at.state.pos + at.state.dir
      if not walls.contains(next_pos) then
        frontier.push(Next(State(next_pos, at.state.dir), at.score + 1))
      end
      frontier.push(Next(State(at.state.pos, at.state.dir.left()), at.score + 1000))
      frontier.push(Next(State(at.state.pos, at.state.dir.right()), at.score + 1000))
    end
    0

  fun count_visited(): U64 =>
    let scores = Map[State, U64]
    let preds = Map[State, Set[State]]
    var solved = false
    var farthest: U64 = 0
    let frontier = MinHeap[Next](0)
    frontier.push(Next(State(start, East), 0))
    while frontier.size() != 0 do
      let at = try frontier.pop()? else break end
      if (not solved) and (at.state.pos == goal) then
        solved = true
        farthest = at.score
      end
      if solved and (at.score > farthest) then
        return backtrack(preds)
      end

      // try matching forward
      let forward = State(at.state.pos + at.state.dir, at.state.dir)
      let forward_score = at.score + 1
      if not walls.contains(forward.pos) then
        try
          let existing = scores(forward)?
          // we've already journeyed this way
          if existing == forward_score then
            // track that another predecessor can reach it just as quickly
            preds.insert_if_absent(forward, Set[State]).set(at.state)
          end
        else
          // walk this way
          scores(forward) = forward_score
          preds.insert_if_absent(forward, Set[State]).set(at.state)
          frontier.push(Next(forward, forward_score))
        end
      end

      for new_dir in [at.state.dir.left(); at.state.dir.right()].values() do
        let turn = State(at.state.pos, new_dir)
        let turn_score = at.score + 1000
        if not scores.contains(turn) then
          scores(turn) = turn_score
          preds.insert_if_absent(turn, Set[State]).set(at.state)
          frontier.push(Next(turn, turn_score))
        end
      end
    end
    0

  fun backtrack(preds: Map[State, Set[State]]): U64 =>
    let seen = Set[State]
    let points = Set[Point]
    let frontier = Array[State]
    for dir in [North; South; East; West].values() do
      frontier.push(State(goal, dir))
    end
    while frontier.size() != 0 do
      let next = try frontier.pop()? else break end
      if seen.contains(next) then continue end
      seen.set(next)
      points.set(next.pos)
      try
        for pred in preds(next)?.values() do
          frontier.push(pred)
        end
      end
    end
    points.size().u64()


actor Day16
  be part1(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    let score = grid.navigate()
    out.print("part 1: " + score.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let grid = parse_grid(input)
    let score = grid.count_visited()
    out.print("part 2: " + score.string())

  fun parse_grid(input: Array[String] val): Grid =>
    var start = Point(0, 0)
    var goal = Point(0, 0)
    let walls = Set[Point]
    for (y, row) in input.pairs() do
      for (x, cell) in Iter[U8](row.values()).collect(Array[U8]).pairs() do
        let point = Point(x.i64(), y.i64())
        if cell == '#' then
          walls.set(point)
        elseif cell == 'S' then
          start = point
        elseif cell == 'E' then
          goal = point
        end
      end
    end
    Grid(start, goal, walls)

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

    Day16.part1(input', env.out)
    Day16.part2(input', env.out)