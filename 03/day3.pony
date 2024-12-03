use "files"

primitive Mul
primitive Do
primitive Dont

class Parser
  let _input: String
  var _index: ISize
  var enabled: Bool
  new create(input: String val) =>
    _input = input
    _index = 0
    enabled = true

  fun ref next_mul(out: OutStream): U32 ? =>
    while _index < _input.size().isize() do
      match _consume_next_instruction()?
      | Do => enabled = true
      | Dont => enabled = false
      | Mul =>
        try
          let left = _consume_number()?
          _consume_literal(",")?
          let right = _consume_number()?
          _consume_literal(")")?
          return left * right
        end
      end
    end
    error

  fun ref _consume_next_instruction(): (Mul | Do | Dont) ? =>
    let mul_index = _input.find("mul(", _index)?
    try
      let do_index = _input.find("do", _index)?
      if do_index < mul_index then
        _index = do_index
        try
          _consume_literal("do()")?
          return Do
        end
        try
          _consume_literal("don't()")?
          return Dont
        end
      end
    end
    _index = mul_index
    _consume_literal("mul(")?
    Mul

  fun ref _consume_number(): U32 ? =>
    (let value, let len) = _input.read_int[U32](_index)?
    if (len < 1) or (len > 3) then error end
    _index = _index + len.isize()
    value

  fun ref _consume_literal(literal: String) ? =>
    if not _input.at(literal, _index) then error end
    _index = _index + literal.size().isize()

actor Day3
  be part1(input: String val, out: OutStream) =>
    var result: U32 = 0
    let parser = Parser(input)
    while true do
      let mul = try parser.next_mul(out)? else break end
      result = result + mul
    end
    out.print("part 1: " + result.string())

  be part2(input: String val, out: OutStream) =>
    var result: U32 = 0
    let parser = Parser(input)
    while true do
      let mul = try parser.next_mul(out)? else break end
      if parser.enabled then
        result = result + mul
      end
    end
    out.print("part 2: " + result.string())


  fun find_mul(input: String val, from: ISize): U32 =>
    let next_number_start = try
      input.find("mul(", from)? + "mul(".size().isize()
    else
      return 0
    end
    next_number_start.u32()

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
    Day3.part1(input', env.out)
    Day3.part2(input', env.out)