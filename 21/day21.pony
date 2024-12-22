use "collections"
use "files"
use "itertools"

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

interface val Move is (Hashable & Stringable)
primitive Up is Move
  fun hash(): USize => 0
  fun string(): String iso^ => "^".clone()
primitive Down is Move
  fun hash(): USize => 1
  fun string(): String iso^ => "v".clone()
primitive Left is Move
  fun hash(): USize => 2
  fun string(): String iso^ => "<".clone()
primitive Right is Move
  fun hash(): USize => 3
  fun string(): String iso^ => ">".clone()
primitive Activate is Move
  fun hash(): USize => 4
  fun string(): String iso^ => "A".clone()

interface Keypad[T: Any val]
  fun start(): Point
  fun position(of: T): Point
  fun is_hfirst_valid(from: Point, to: Point): Bool
  fun is_vfirst_valid(from: Point, to: Point): Bool

class Solver[T: Any val]
  let pad: Keypad[T] 
  new create(pad': Keypad[T]) =>
    pad = pad'

  fun apply(goal: Iterator[T]): Array[Array[Move]] =>
    var all_paths = Array[Array[Move]]
    all_paths.push([])
    var pos = pad.start()
    for char in goal do
      let target = pad.position(char)
      let paths' = Array[Array[Move]]
      for new_piece in _paths(pos, target).values() do
        for old_piece in all_paths.values() do
          let path' = old_piece.clone()
          path'.concat(new_piece.values())
          paths'.push(path')
        end
      end
      pos = target
      all_paths = paths'
    end
    all_paths

  fun paths(from: T, to: T): Array[Array[Move] val] =>
    _paths(pad.position(from), pad.position(to))

  fun _paths(from: Point, to: Point): Array[Array[Move] val] =>
    let dx = to.x - from.x
    let dy = to.y - from.y
    if (dx == 0) and (dy == 0) then
      return [[Activate]]
    elseif dx == 0 then
      return [_move_vh(from, to)]
    elseif dy == 0 then
      return [_move_hv(from, to)]
    end
    let pieces = Array[Array[Move] val]
    if pad.is_hfirst_valid(from, to) then
      pieces.push(_move_hv(from, to))
    end
    if pad.is_vfirst_valid(from, to) then
      pieces.push(_move_vh(from, to))
    end
    pieces

  fun _move_hv(from: Point, to: Point): Array[Move] val =>
    let moves = Iter[Move].chain([
      _move_directionally(to.x - from.x, Left, Right)
      _move_directionally(to.y - from.y, Up, Down)
      Iter[Move].maybe(Activate)
    ].values())
    let result: Array[Move] iso = Array[Move]
    for move in moves do result.push(move) end
    recover val consume result end

  fun _move_vh(from: Point, to: Point): Array[Move] val =>
    let moves = Iter[Move].chain([
      _move_directionally(to.y - from.y, Up, Down)
      _move_directionally(to.x - from.x, Left, Right)
      Iter[Move].maybe(Activate)
    ].values())
    let result: Array[Move] iso = Array[Move]
    for move in moves do result.push(move) end
    recover val consume result end

  fun _move_directionally(delta: I64, neg: Move, pos: Move): Iter[Move] =>
    if delta < 0 then
      Iter[Move].repeat_value(neg).take(delta.abs().usize())
    else
      Iter[Move].repeat_value(pos).take(delta.usize())
    end

class Numpad is Keypad[U8]
  fun start(): Point => position('A')
  fun position(of: U8): Point =>
    match of
    | 'A' => Point(2, 3)
    | '0' => Point(1, 3)
    | '1' => Point(0, 2)
    | '2' => Point(1, 2)
    | '3' => Point(2, 2)
    | '4' => Point(0, 1)
    | '5' => Point(1, 1)
    | '6' => Point(2, 1)
    | '7' => Point(0, 0)
    | '8' => Point(1, 0)
    | '9' => Point(2, 0)
    else Point(-1, -1)
    end
  fun is_hfirst_valid(from: Point, to: Point): Bool =>
    (from.y != 3) or (to.x != 0)
  fun is_vfirst_valid(from: Point, to: Point): Bool =>
    (from.x != 0) or (to.y != 3)

class DPad is Keypad[Move]
  fun start(): Point => position(Activate)
  fun position(of: Move): Point =>
    match of
    | Left => Point(0, 1)
    | Up => Point(1, 0)
    | Down => Point(1, 1)
    | Activate => Point(2, 0)
    | Right => Point(2, 1)
    else Point(-1, -1)
    end
  fun is_hfirst_valid(from: Point, to: Point): Bool =>
    (from.y != 0) or (to.x != 0)
  fun is_vfirst_valid(from: Point, to: Point): Bool =>
    (from.x != 0) or (to.y != 0)

class val Segment is (Hashable & Equatable[Segment])
  let from: Move
  let to: Move
  new val create(from': Move, to': Move) =>
    from = from'
    to = to'
  
  fun hash(): USize =>
    var h: USize = 7
    h = (h * 31) + from.hash()
    h = (h * 31) + to.hash()
    h

  fun eq(that: box->Segment): Bool =>
    (from is that.from) and (to is that.to)

class val State is (Hashable & Equatable[State])
  let from: Move
  let to: Move
  let depth: U64
  new val create(from': Move, to': Move, depth': U64) =>
    from = from'
    to = to'
    depth = depth'

  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + from.hash()
    h = (31 * h) + to.hash()
    h = (31 * h) + depth.hash()
    h

  fun eq(that: box->State): Bool =>
    ((from is that.from) and (to is that.to)) and (depth == that.depth)

class Solution
  let solve_numpad: Solver[U8] = Solver[U8](Numpad)
  let solve_dpad: Solver[Move] = Solver[Move](DPad)
  let cache: Map[State, U64] = Map[State, U64]

  let moves: Map[Segment, Array[Move] val]
  new create(moves': Map[Segment, Array[Move] val]) =>
    moves = moves'

  fun ref solve(goal: String, bots: U64): U64 =>
    let goal_array = Iter[U8](goal.values()).collect(Array[U8])
    var total: U64 = 0
    for (i, to) in goal_array.pairs() do
      let from = try goal_array(i - 1)? else 'A' end
      var min: U64 = U64.max_value()
      for path in solve_numpad.paths(from, to).values() do
        let local_min = _solve(path, bots)
        if local_min < min then min = local_min end
      end
      total = total + min
    end
    total
  
  fun ref _solve(seg: Array[Move] val, bots: U64): U64 =>
    if bots == 0 then
      return seg.size().u64()
    end
    var sum: U64 = 0
    for (i, to) in seg.pairs() do
      let from = try seg(i - 1)? else Activate end
      sum = sum + _solve2(from, to, bots - 1)
    end
    sum

  fun ref _solve2(from: Move, to: Move, bots: U64): U64 =>
    let cache_key = State(from, to, bots)
    try
      return cache(cache_key)?
    end
    let solution = try
      let seg = moves(Segment(from, to))?
      _solve(seg, bots)
    else
      U64.max_value() // should not be possible
    end
    cache(cache_key) = solution
    solution

class SolutionExplorer
  let move_sets: Array[Map[Segment, Array[Move] val]]
  new create() =>
    let solve_dpad = Solver[Move](DPad)

    var sets = Array[Map[Segment, Array[Move] val]]
    sets.push(Map[Segment, Array[Move] val])
    for from in [Up; Down; Left; Right; Activate].values() do
      for to in [Up; Down; Left; Right; Activate].values() do
        let segment = Segment(from, to)
        let sets' = Array[Map[Segment, Array[Move] val]]
        for path in solve_dpad.paths(from, to).values() do
          for set in sets.values() do
            let set' = set.clone()
            set'(segment) = path
            sets'.push(set')
          end
        end
        sets = sets'
      end
    end
    move_sets = sets


  fun solve(goal: String, bots: U64): U64 =>
    var min: U64 = U64.max_value()
    for move_set in move_sets.values() do
      let local_min = Solution(move_set.clone()).solve(goal, bots)
      if local_min < min then
        min = local_min
      end
    end
    min

actor Day21
  be part1(input: Array[String] val, out: OutStream) =>
    let solve_numpad = Solver[U8](Numpad)
    let solve_dpad = Solver[Move](DPad)
    var answer: U64 = 0
    for line in input.values() do
      var shortest_len: USize = USize.max_value()
      for solution in solve_numpad(line.values()).values() do
        for solution' in solve_dpad(solution.values()).values() do
          for full_solution in solve_dpad(solution'.values()).values() do
            if full_solution.size() < shortest_len then
              shortest_len = full_solution.size()
            end
          end
        end
      end
      let num_val = try line.substring(0, 3).u64()? else 0 end
      answer = answer + (num_val * shortest_len.u64())
    end
    out.print("part 1: " + answer.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let solve_numpad = Solver[U8](Numpad)
    let solve_dpad = Solver[Move](DPad)
    let explorer = SolutionExplorer
    var answer: U64 = 0
    for line in input.values() do
      let shortest_len = explorer.solve(line, 25)
      let num_val = try line.substring(0, 3).u64()? else 0 end
      answer = answer + (num_val * shortest_len)
    end
    out.print("part 2: " + answer.string())
    None

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

    Day21.part1(input', env.out)
    Day21.part2(input', env.out)