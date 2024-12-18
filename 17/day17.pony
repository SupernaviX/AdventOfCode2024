use "collections"
use "files"

class Program
  let program: Array[U64]
  new create(program': Array[U64]) =>
    program = program'
  
  fun run(a: U64, b: U64, c: U64): Array[U64] =>
    try
      State(a, b, c, program).run()?
    else
      Array[U64]
    end

  fun size(): USize =>
    program.size()

class State
  var reg_a: U64
  var reg_b: U64
  var reg_c: U64
  let program: Array[U64] box
  var ip: USize
  let output: Array[U64]
  new create(a: U64, b: U64, c: U64, program': Array[U64] box) =>
    reg_a = a
    reg_b = b
    reg_c = c
    program = program'
    ip = 0
    output = Array[U64]

  fun ref run(): Array[U64] ? =>
    while ip < program.size() do
      step()?
    end
    output

  fun ref step() ? =>
    let opcode = program(ip)?
    match opcode
    | 0 => _adv()?
    | 1 => _bxl()?
    | 2 => _bst()?
    | 3 => _jnz()?
    | 4 => _bxc()
    | 5 => _out()?
    | 6 => _bdv()?
    | 7 => _cdv()?
    else error
    end

  fun ref _adv() ? =>
    let operand = _combo_operand()?
    reg_a = reg_a >> operand
    ip = ip + 2

  fun ref _bxl() ? =>
    let operand = _literal_operand()?
    reg_b = operand xor reg_b
    ip = ip + 2

  fun ref _bst() ? =>
    let operand = _combo_operand()?
    reg_b = operand % 8
    ip = ip + 2

  fun ref _jnz() ? =>
    let operand = _literal_operand()?
    if reg_a != 0 then
      ip = operand.usize()
    else
      ip = ip + 2
    end

  fun ref _bxc() =>
    reg_b = reg_b xor reg_c
    ip = ip + 2
  
  fun ref _out() ? =>
    let operand = _combo_operand()?
    output.push(operand % 8)
    ip = ip + 2

  fun ref _bdv() ? =>
    let operand = _combo_operand()?
    reg_b = reg_a >> operand
    ip = ip + 2

  fun ref _cdv() ? =>
    let operand = _combo_operand()?
    reg_c = reg_a >> operand
    ip = ip + 2

  fun _literal_operand(): U64 ? =>
    program(ip + 1)?

  fun _combo_operand(): U64 ? =>
    match program(ip + 1)?
    | 0 => 0
    | 1 => 1
    | 2 => 2
    | 3 => 3
    | 4 => reg_a
    | 5 => reg_b
    | 6 => reg_c
    else error
    end

class Quinifier
  let terms: Array[U64]
  let program: Program
  new create(terms': Array[U64]) =>
    terms = terms'
    program = Program(terms)

  fun solve(): U64 ? =>
    _solve_rest(0, terms.size())?
  
  fun _solve_rest(prefix: U64, digits: USize): U64 ? =>
    let expected = Array[U64]
    terms.copy_to(expected, digits - 1, 0, (terms.size() - digits) + 1)
    for i in Range(0, 8) do
      let a' = (prefix << 3) + i.u64()
      let actual = program.run(a', 0, 0)
      if not array_eq(expected, actual) then
        continue
      end
      if digits == 1 then
        return a'
      end
      try
        return _solve_rest(a', digits - 1)?
      end
    end
    error

  fun array_eq(expected: Array[U64], actual: Array[U64]): Bool =>
    if expected.size() != actual.size() then return false end
    try
      for (i, e) in expected.pairs() do
        if e != actual(i)? then return false end
      end
    else
      false
    end
    true

actor Day17
  be part1(input: Array[String] val, out: OutStream) =>
    try
      (let a, let b, let c, let program) = parse(input)?
      let output = Program(program).run(a, b, c)
      out.print("part 1: " + ",".join(output.values()))
    end

  /*
    bst a
    bxl 5
    cdv b
    bxc
    adv 3
    bxl 6
    out b
    jnz 0

    var a: U64 = ???;
    var b: U64 = 0;
    var c: U64 = 0;
    do {
      b = a % 8;
      b = b xor 5;
      c = a >> b;
      b = b xor c;
      a = a >> 3;
      b = 6 xor b;
      out(b);
    } while (a);

    do {
      b = (a % 8) xor 5;
      b = b xor (a >> b) xor 6;
      out(b);
      a = a >> 3;
    } while (a);

    b0 = (a % 8) xor 5;
    out[0] = b0 xor (a >> b0) xor 6;
    b1 = ((a >> 3) % 8) xor 5;
    out[1] = b1 xor (a >> 3 >> b1) xor 6;

    b[n] = ((a >> 3n) % 8) xor 5;
    out[n] = b[n] xor (a >> (3n + b[n])) xor 6;

   */
  be part2(input: Array[String] val, out: OutStream) =>
    try
      (_, _, _, let terms) = parse(input)?
      let a = Quinifier(terms).solve()?
      out.print("part 2: " + a.string())
    else
      out.print("part 2 failed")
    end

  fun parse(input: Array[String] val): (U64, U64, U64, Array[U64]) ? =>
    let a = input(0)?.substring("Register A: ".size().isize()).u64()?
    let b = input(1)?.substring("Register B: ".size().isize()).u64()?
    let c = input(2)?.substring("Register C: ".size().isize()).u64()?
    let program = Array[U64]
    for code in input(4)?.substring("Program: ".size().isize()).split(",").values() do
      program.push(code.u64()?)
    end
    (a, b, c, program)

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

    Day17.part1(input', env.out)
    Day17.part2(input', env.out)