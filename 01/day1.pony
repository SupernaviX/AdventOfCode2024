use "collections"
use "files"
use "itertools"

actor Day1
  be part1(input: Array[String] val, out: OutStream) =>
    (let list1, let list2) = try
      parse_input(input)?
    else
      out.print("malformed input")
      return
    end
    Sort[Array[U32], U32](list1)
    Sort[Array[U32], U32](list2)
    let result = Iter[U32](list1.values())
      .zip[U32](list2.values())
      .map[U32]({(x) => (x._1.i32() - x._2.i32()).abs() })
      .fold[U32](0, {(sum, x) => sum + x})
    out.print("part 1: " + result.string())

  be part2(input: Array[String] val, out: OutStream) =>
    (let list1, let list2) = try
      parse_input(input)?
    else
      out.print("malformed input")
      return
    end

    let frequencies = Map[U32, U32]
    for number in list2.values() do
      frequencies.upsert(number, 1, {(sum, inc) => sum + inc})
    end
    var similarity: U32 = 0
    for number in list1.values() do
      let freq = frequencies.get_or_else(number, 0)
      similarity = similarity + (number * freq)
    end
    out.print("part 2: " + similarity.string())
  
  fun parse_input(input: Array[String] val): (Array[U32], Array[U32]) ? =>
    let list1 = Array[U32]
    let list2 = Array[U32]
    for line in input.values() do
      let numbers = Iter[String](line.split().values())
        .filter({(v) => v.size() > 0})

      list1.push(numbers.next()?.u32()?)
      list2.push(numbers.next()?.u32()?)
    end
    (list1, list2)


actor Main
  new create(env: Env) =>
    let input: Array[String] iso = Array[String]
    try
      let filename = env.args(1)?
      let path = FilePath(FileAuth(env.root), filename)
      match OpenFile(path)
      | let file: File =>
        for line in file.lines() do 
          input.push(consume line)
        end
      else
        env.err.print("Could not open file")
        return
      end
    else
      env.err.print("Invalid path")
      return
    end
    let input': Array[String] val = recover val
      consume input
    end
    Day1.part1(input', env.out)
    Day1.part2(input', env.out)
