use "collections"
use "files"

actor Day9
  be part1(input: String val, out: OutStream) =>
    let fs = parse_fs(input)
    let blocks = parse_blocks(fs)
    compact_blocks(blocks)
    out.print("part 1: " + checksum(blocks).string())

  be part2(input: String val, out: OutStream) =>
    let fs = parse_fs(input)
    compact_fs(fs)
    let blocks = parse_blocks(fs)
    out.print("part 2: " + checksum(blocks).string())

  fun parse_fs(input: String val): Array[((U64 | None), USize)] =>
    let res = Array[((U64 | None), USize)]
    var empty = false
    var index: U64 = 0
    for char in input.values() do
      let count = (char - '0').usize()
      res.push((if empty then None else index end, count))
      if empty then
        empty = false
      else
        index = index + 1
        empty = true
      end
    end
    res

  fun parse_blocks(fs: Array[((U64 | None), USize)]): Array[(U64 | None)] =>
    let res = Array[(U64 | None)]
    for (index, count) in fs.values() do
      for i in Range(0, count) do
        res.push(index)
      end
    end
    res

  fun compact_blocks(blocks: Array[(U64 | None)]) =>
    var left: USize = 0
    var right: USize = blocks.size() - 1
    try
      while left < right do
        while not (blocks(left)? is None) do
          left = left + 1
        end
        while blocks(right)? is None do
          right = right - 1
        end
        if left >= right then break end
        blocks(left)? = blocks(right)?
        blocks(right)? = None
      end
    end

  fun compact_fs(fs: Array[((U64 | None), USize)]) =>
    var right: USize = fs.size() - 1
    var next_to_compact: U64 = 0
    try
      while right > 0 do
        next_to_compact = match fs(right)?._1
        | let index: U64 => index
        else
          right = right - 1
          continue
        end
        break
      end
    end

    try
      while next_to_compact > 0 do
        (let r_index, let r_count) = fs(right)?
        match r_index
        | next_to_compact => None
        else
          right = right - 1
          continue
        end

        for left in Range(0, right) do
          (let l_index, let l_count) = fs(left)?
          if not ((l_index is None) and (l_count >= r_count)) then
            continue
          end

          fs(left)? = (r_index, r_count)
          if l_count > r_count then
            fs.insert(left + 1, (None, l_count - r_count))?
            right = right + 1
          end

          fs(right)? = (None, r_count)

          break
        end

        next_to_compact = next_to_compact - 1
      end
    end
    None
  
  fun checksum(blocks: Array[(U64 | None)]): U64 =>
    var sum: U64 = 0
    for (index, value) in blocks.pairs() do
      sum = sum + match value
      | let num: U64 => index.u64() * num
      else 0
      end
    end
    sum

actor Main
  new create(env: Env) =>
    let filename = try
      env.args(1)?
    else
      env.err.print("Invalid path")
      return
    end
    let path = FilePath(FileAuth(env.root), filename)
    let input = match OpenFile(path)
      | let file: File => file.read_string(file.size())
      else
        env.err.print("Could not read file")
        return
      end
    let input': String val = recover val
      consume input
    end
    Day9.part1(input', env.out)
    Day9.part2(input', env.out)