use "files"
use "itertools"

actor Day2
  be part1(input: Array[String] val, out: OutStream) =>
    let reports = try
      parse_input(input)?
    else
      out.print("malformed input")
      return
    end
    var sum: U32 = 0
    for report in reports.values() do
      if is_report_safe(report) then
        sum = sum + 1
      end
    end
    out.print("part 1: " + sum.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let reports = try
      parse_input(input)?
    else
      out.print("malformed input")
      return
    end
    var sum: U32 = 0
    for report in reports.values() do
      if is_report_almost_safe(report) then
        sum = sum + 1
      end
    end
    out.print("part 2: " + sum.string())

  fun is_report_almost_safe(report: Array[I32]): Bool =>
    if is_report_safe(report) then
      return true
    end
    for skip in report.keys() do
      let shorter_report = remove_at(report, skip)
      if is_report_safe(shorter_report) then
        return true
      end
    end
    false

  fun is_report_safe(report: Array[I32]): Bool =>
    var positive = false
    var negative = false
    for delta in find_deltas(report) do
      if delta > 0 then
        if negative then return false end
        if delta > 3 then return false end
        positive = true
      elseif delta < 0 then
        if positive then return false end
        if delta < -3 then return false end
        negative = true
      else
        return false
      end
    end
    true

  fun find_deltas(elements: Array[I32]): Iter[I32] =>
    Iter[I32](elements.values())
      .skip(1)
      .zip[I32](elements.values())
      .map[I32]({(x) => (x._1 - x._2)})

  fun remove_at(elements: Array[I32], index: USize): Array[I32] =>
    Iter[I32].chain([
      Iter[I32](elements.values()).take(index)
      Iter[I32](elements.values()).skip(index + 1)
    ].values()).collect(Array[I32])

  fun parse_input(input: Array[String] val): Array[Array[I32]] ? =>
    let list = Array[Array[I32]]
    for levels in input.values() do
      let report = Array[I32]
      for level in levels.split().values() do
        report.push(level.i32()?)
      end
      list.push(report)
    end
    list

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
    Day2.part1(input', env.out)
    Day2.part2(input', env.out)