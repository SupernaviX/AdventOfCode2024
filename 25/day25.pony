use "collections"
use "files"
use "itertools"

actor Day25
  be part1(input: Array[String] val, out: OutStream) =>
    (let locks, let keys) = parse_input(input)
    var combos: U64 = 0
    for lock in locks.values() do
      for key in keys.values() do
        if fits(lock, key) then
          combos = combos + 1
        end
      end
    end
    out.print("part 1: " + combos.string())
  
  fun fits(lock: Array[U64], key: Array[U64]): Bool =>
    for (i, lock_bit) in lock.pairs() do
      let key_bit = try key(i)? else return false end
      if (lock_bit + key_bit) > 5 then return false end
    end
    true

  fun parse_input(input: Array[String] val): (Array[Array[U64]], Array[Array[U64]]) =>
    let locks = Array[Array[U64]]
    let keys = Array[Array[U64]]
    let lines = input.reverse()
    try
      while lines.size() != 0 do
        let sizes: Array[U64] = [0; 0; 0; 0; 0]
        let is_lock = lines(lines.size() - 1)? == "#####"
        while lines.size() != 0 do
          let line = lines.pop()?
          if line.size() == 0 then break end
          for (i, char) in Iter[U8](line.values()).collect(Array[U8]).pairs() do
            if char == '#' then
              sizes(i)? = sizes(i)? + 1
            end
          end
        end
        for (i, size) in sizes.pairs() do
          sizes(i)? = sizes(i)? - 1
        end
        if is_lock then
          locks.push(sizes)
        else
          keys.push(sizes)
        end
      end
    end
    (locks, keys)

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

    Day25.part1(input', env.out)
