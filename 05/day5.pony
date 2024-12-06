use "collections"
use "files"
use "itertools"

primitive Before
primitive After

type Rule is (Before | After, U64)

class Input
  let rules: Array[(U64, U64)]
  let pages: Array[Array[U64]]
  new create(rules': Array[(U64, U64)], pages': Array[Array[U64]]) =>
    rules = rules'
    pages = pages'

actor Day5
  be part1(input: Array[String] val, out: OutStream) =>
    let input' = parse_input(input)
    let rules = Map[U64, Array[Rule]]
    for (before, after) in input'.rules.values() do
      rules.insert_if_absent(before, Array[Rule]).push((Before, after))
      rules.insert_if_absent(after, Array[Rule]).push((After, before))
    end
    var total: U64 = 0
    for pages in input'.pages.values() do
      if follows_rules(rules, pages) then
        total = total + middle_page(pages)
      end
    end
    out.print("part 1: " + total.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let input' = parse_input(input)
    let rules = Map[U64, Array[Rule]]
    for (before, after) in input'.rules.values() do
      rules.insert_if_absent(before, Array[Rule]).push((Before, after))
      rules.insert_if_absent(after, Array[Rule]).push((After, before))
    end
    var total: U64 = 0
    for pages in input'.pages.values() do
      if not follows_rules(rules, pages) then
        make_follow_rules(rules, pages)
        total = total + middle_page(pages)
      end
    end
    out.print("part 2: " + total.string())

  fun follows_rules(rules: Map[U64, Array[Rule]], pages: Array[U64]): Bool =>
    try
      find_rulebreaker(rules, pages)?
      false
    else
      true
    end
  
  fun find_rulebreaker(rules: Map[U64, Array[Rule]], pages: Array[U64]): (U64, Rule) ? =>
    let pages' = Map[U64, USize]
    for (index, page) in pages.pairs() do
      pages'.insert(page, index)
    end
    for (index, page) in pages.pairs() do
      let rules' = try rules(page)? else continue end
      for (direction, page') in rules'.values() do
        try
          let index' = pages'(page')?
          let satisfied = match direction
            | Before => index < index'
            | After => index > index'
            end
          if not satisfied then
            return (page, (direction, page'))
          end
        end
      end
    end
    error

  fun make_follow_rules(rules: Map[U64, Array[Rule]], pages: Array[U64]): Array[U64] =>
    while true do
      try
        (let page, (let dir, let page')) = find_rulebreaker(rules, pages)?
        let index = pages.find(page)?
        let index' = pages.find(page')?
        pages.swap_elements(index, index')?
      else
        break
      end
    end
    pages

  fun middle_page(pages: Array[U64]): U64 =>
    try pages(pages.size() / 2)? else 0 end

  fun parse_input(input: Array[String] val): Input =>
    let rules = Array[(U64, U64)]
    let pages = Array[Array[U64]]
    for line in input.values() do
      if line.size() == 0 then
        continue
      end
      try
        let split = line.split("|")
        let first = split(0)?.u64()?
        let second = split(1)?.u64()?
        rules.push((first, second))
      else
        let nums = Iter[String](line.split(",").values())
          .filter_map[U64]({(v) => try v.u64()? else None end})
          .collect(Array[U64])
        pages.push(nums)
      end
    end
    Input(rules, pages)

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

    Day5.part1(input', env.out)
    Day5.part2(input', env.out)
