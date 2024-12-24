use "collections"
use "files"
use "format"

primitive AndGate
  fun apply(l: Bool, r: Bool): Bool => l and r
  fun hash(): USize => 0
primitive OrGate
  fun apply(l: Bool, r: Bool): Bool => l or r
  fun hash(): USize => 1
primitive XorGate
  fun apply(l: Bool, r: Bool): Bool => l xor r
  fun hash(): USize => 2
type GateKind is (AndGate | OrGate | XorGate)

class val Gate is (Hashable & Equatable[Gate])
  let kind: GateKind
  let left: String
  let right: String
  new create(kind': GateKind, left': String, right': String) =>
    kind = kind'
    left = left'
    right = right'

  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + kind.hash()
    h = (31 * h) + left.hash()
    h = (31 * h) + right.hash()
    h

  fun eq(that: box->Gate): Bool =>
    ((kind is that.kind) and (left == that.left)) and (right == that.right)

class val WirePair
  let left: String
  let right: String
  new create(left': String, right': String) =>
    left = left'
    right = right'

class Circuit
  let gates: Map[String, Gate]
  let outputs: Map[Gate, String]
  new create(gates': Map[String, Gate]) =>
    gates = gates'
    outputs = Map[Gate, String]
    for (output, gate) in gates.pairs() do
      outputs(gate) = output
    end

  fun solve(start: Map[String, Bool]): U64 =>
    var answer: U64 = 0
    let solved = start.clone()
    var bit: U64 = 0
    while true do
      let out: String = wire_for("z", bit)
      try
        let value = solve_for(solved, out)?
        if value then
          answer = answer + (U64(1) << bit)
        end
        bit = bit + 1
      else
        break
      end
    end
    answer

  fun solve_for(solved: Map[String, Bool], out: String): Bool ? =>
    try
      return solved(out)?
    end
    let gate = gates(out)?
    let left = solve_for(solved, gate.left)?
    let right = solve_for(solved, gate.right)?
    let value = gate.kind(left, right)
    solved(out) = value
    value

  fun find_replacements(): Map[String, String] =>
    let swapped = Map[String, String]
    var last_carry_wire: (String | None) = None
    var bit: U64 = 0
    while true do
      try
        let x_wire = wire_for("x", bit)
        let y_wire = wire_for("y", bit)
        let z_wire = wire_for("z", bit)
        let total_wire = output_for(XorGate, x_wire, y_wire, swapped)?
        let carry_wire = output_for(AndGate, x_wire, y_wire, swapped)?
        let z_gate = gates(z_wire)?

        let last_carry = match last_carry_wire
          | let carry: String => carry
          else
            last_carry_wire = carry_wire
            bit = bit + 1
            continue
        end

        try
          let digit_wire = output_for(XorGate, total_wire, last_carry, swapped)?
          // We have the right value for the current digit, it's just wired to the wrong place.
          // Fix that.
          if digit_wire != z_wire then
            swapped(digit_wire) = z_wire
            swapped(z_wire) = digit_wire
          end
        else
          // It's possible that the carry is wired wrong. See if swapping it fixes that.
          swapped(total_wire) = carry_wire
          swapped(carry_wire) = total_wire

          let digit_wire = output_for(XorGate, total_wire, last_carry, swapped)?
          if digit_wire != z_wire then
            return swapped
          end
        end

        let intermediate = output_for(AndGate, total_wire, last_carry, swapped)?
        last_carry_wire = output_for(OrGate, intermediate, carry_wire, swapped)?

        bit = bit + 1        
      else
        break
      end
    end
    swapped

  fun wire_for(letter: String, digit: U64): String =>
    letter + Format.int[U64](digit where width = 2, fill = '0')

  fun output_for(kind: GateKind, left: String, right: String, swapped: Map[String, String]): String ? =>
    let left' = try swapped(left)? else left end
    let right' = try swapped(right)? else right end
    try
      return outputs(Gate(kind, left', right'))?
    end
    try
      return outputs(Gate(kind, right', left'))?
    end
    error

actor Day24
  be part1(input: Array[String] val, out: OutStream) =>
    (let circuit, let start) = parse_input(input)
    let answer = circuit.solve(start)
    out.print("part 1: " + answer.string())

  be part2(input: Array[String] val, out: OutStream) =>
    (let circuit, _) = parse_input(input)
    let replacements = circuit.find_replacements()
    let replaced = Array[String]
    for replacement in replacements.values() do
      replaced.push(replacement)
    end
    Sort[Array[String], String](replaced)
    out.print("part 2: " + ",".join(replaced.values()))
    None

  fun parse_input(input: Array[String] val): (Circuit, Map[String, Bool]) =>
    let gates = Map[String, Gate]
    let start = Map[String, Bool]
    for line in input.values() do
      if line.at(":", 3) then
        (let wire, let value) = line.clone().chop(3)
        let wire' = recover val wire.clone() end
        let value' = value.at("1", 2)
        start(wire') = value'
      else
        let left = recover val line.substring(0, 3) end
        let kind: GateKind =
          if line.at("AND", 4) then
            AndGate
          elseif line.at("OR", 4) then
            OrGate
          elseif line.at("XOR", 4) then
            XorGate
          else
            continue
          end
        let rest = line.substring(if kind is OrGate then 7 else 8 end)
        let right = recover val rest.substring(0, 3) end
        let out = recover val rest.substring(7) end
        gates(out) = Gate(kind, left, right)
      end
    end
    (Circuit(gates), start)


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

    Day24.part1(input', env.out)
    Day24.part2(input', env.out)