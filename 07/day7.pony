use "files"

class Equation
  let result: U64
  let operands: Array[U64]
  new create(result': U64, operands': Array[U64]) =>
    result = result'
    operands = operands'

actor Day7
  be part1(input: Array[String] val, out: OutStream) =>
    var total: U64 = 0
    for line in input.values() do
      let equation = try
        parse_equation(line)?
      else
        out.print("malformed input")
        return
      end
      
      if has_solution(equation) then
        total = total + equation.result
      end
    end
    out.print("part 1: " + total.string())

  be part2(input: Array[String] val, out: OutStream) =>
    var total: U64 = 0
    for line in input.values() do
      let equation = try
        parse_equation(line)?
      else
        out.print("malformed input")
        return
      end
      
      if has_solution_with_concat(equation) then
        total = total + equation.result
      end
    end
    out.print("part 2: " + total.string())

  fun parse_equation(equation: String): Equation ? =>
    let parts = equation.split_by(": ")
    let result = parts(0)?.u64()?
    let operands = Array[U64]
    for operand in parts(1)?.split(" ").values() do
      operands.push(operand.u64()?)
    end
    Equation(result, operands)

  fun has_solution(equation: Equation): Bool =>
    let sofar = try equation.operands(0)? else return false end
    find_solution(equation, 1, sofar)

  fun find_solution(equation: Equation, index: USize, sofar: U64): Bool =>
    try
      let operand = equation.operands(index)?
      find_solution(equation, index + 1, sofar + operand) or
      find_solution(equation, index + 1, sofar * operand)
    else
      equation.result == sofar
    end

  fun has_solution_with_concat(equation: Equation): Bool =>
    let sofar = try equation.operands(0)? else return false end
    find_solution_with_concat(equation, 1, sofar)

  fun find_solution_with_concat(equation: Equation, index: USize, sofar: U64): Bool =>
    try
      let operand = equation.operands(index)?
      find_solution_with_concat(equation, index + 1, sofar + operand) or
      find_solution_with_concat(equation, index + 1, sofar * operand) or
      find_solution_with_concat(equation, index + 1, (sofar.string() + operand.string()).u64()?)
    else
      equation.result == sofar
    end

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

    Day7.part1(input', env.out)
    Day7.part2(input', env.out)