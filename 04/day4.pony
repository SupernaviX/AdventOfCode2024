use "files"
use "itertools"

primitive Directions
  fun all(): Array[(ISize, ISize)] =>
    [
      (-1, -1); (0, -1); (1, -1)
      (-1,  0);          (1,  0)
      (-1,  1); (0,  1); (1,  1)
    ]

  fun diagonal(): Array[(ISize, ISize)] =>
    [
      (-1, -1); (1, -1)
      ( 1, -1); (1,  1)
    ]

actor Day4
  be part1(input: Array[String] val, out: OutStream) =>
    let map = parse_map(input)
    let height = map.size()
    var count: U64 = 0
    for (y, row) in map.pairs() do
      for (x, cell) in row.pairs() do 
        if cell != 'X' then continue end
        for dir in Directions.all().values() do
          if is_xmas(map, (x.isize(), y.isize()), dir) then
            count = count + 1
          end
        end
      end
    end
    out.print("part 1: " + count.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let map = parse_map(input)
    let height = map.size()
    var count: U64 = 0
    for (y, row) in map.pairs() do
      for (x, cell) in row.pairs() do 
        if cell != 'A' then continue end
        if is_x_mas(map, (x.isize(), y.isize())) then
          count = count + 1
        end
      end
    end
    out.print("part 2: " + count.string())

  fun parse_map(input: Array[String] val): Array[Array[U8]] =>
    Iter[String](input.values())
      .map[Array[U8]]({(v) => Iter[U8](v.values()).collect(Array[U8])})
      .collect(Array[Array[U8]])
      
  fun is_xmas(map: Array[Array[U8]], x_pos: (ISize, ISize), step: (ISize, ISize)): Bool =>
    let width = map.size().isize()
    let height = try map(0)?.size().isize() else return false end
    var pos = x_pos
    for next in "MAS".values() do
      pos = (pos._1 + step._1, pos._2 + step._2)
      let char = try char_at(map, pos)? else return false  end
      if char != next then return false end
    end
    true
  
  fun is_x_mas(map: Array[Array[U8]], a_pos: (ISize, ISize)): Bool =>
    try
      let ul = char_at(map, move(a_pos, (-1, -1)))?
      let ur = char_at(map, move(a_pos, ( 1, -1)))?
      let dl = char_at(map, move(a_pos, (-1,  1)))?
      let dr = char_at(map, move(a_pos, ( 1,  1)))?
      (((ul == 'M') and (dr == 'S')) or ((ul == 'S') and (dr == 'M')))
      and (((ur == 'M') and (dl == 'S')) or ((ur == 'S') and (dl == 'M')))
    else
      false
    end

  fun move(pos: (ISize, ISize), step: (ISize, ISize)): (ISize, ISize) =>
    (pos._1 + step._1, pos._2 + step._2)

  fun char_at(map: Array[Array[U8]], pos: (ISize, ISize)): U8 ? =>
    map(pos._2.usize())?(pos._1.usize())?

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

    Day4.part1(input', env.out)
    Day4.part2(input', env.out)
