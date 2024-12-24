use "collections"
use p = "collections/persistent"
use "files"

class val Edge is (Hashable & Equatable[Edge])
  let from: String
  let to: String
  new create(from': String, to': String) =>
    if from' < to' then
      from = from'
      to = to'
    else
      from = to'
      to = from'
    end
  
  fun hash(): USize =>
    var h: USize = 7
    h = (31 * h) + from.hash()
    h = (31 * h) + to.hash()
    h

  fun eq(that: box->Edge): Bool =>
    (from == that.from) and (to == that.to)

class val Cluster is (Hashable & Equatable[Cluster])
  let nodes: Array[String]
  new create(first: String, second: String, third: String) =>
    nodes = Sort[Array[String], String]([first; second; third])
  
  new val extended(old: Cluster val, node: String) =>
    let nodes' = Array[String]
    for old' in old.nodes.values() do
      if old' != node then
        nodes'.push(old')
      end
    end
    nodes'.push(node)
    nodes = Sort[Array[String], String](nodes')
  
  fun hash(): USize =>
    var h: USize = 7
    for node in nodes.values() do
      h = (31 * h) + node.hash()
    end
    h

  fun eq(that: box->Cluster): Bool =>
    if nodes.size() != that.nodes.size() then return false end
    for (i, node) in nodes.pairs() do
      let node' = try that.nodes(i)? else return false end
      if node != node' then return false end
    end
    true

  fun val add(that: String): Cluster =>
    Cluster.extended(this, that)

class Graph
  let nodes: Set[String]
  let edges: Map[String, p.Set[String]]
  new create(nodes': Set[String], edges': Map[String, p.Set[String]]) =>
    nodes = nodes'
    edges = edges'

  fun clusters_of_three(): Set[Cluster] =>
    let clusters = Set[Cluster]
    for node in nodes.values() do
      let peers = edges_for(node)
      for peer in peers.values() do
        let commons = peers and edges_for(peer)
        for common in commons.values() do
          clusters.set(Cluster(node, peer, common))
        end
      end
    end
    clusters

  fun largest_cluster(): Cluster ? =>
    var frontier = Map[Cluster, p.Set[String]]
    for node in nodes.values() do
      let peers = edges_for(node)
      for peer in peers.values() do
        let commons = peers and edges_for(peer)
        for common in commons.values() do
          frontier(Cluster(node, peer, common)) = commons and edges_for(common)
        end
      end
    end
    var count: USize = 3
    while frontier.size() > 1 do
      let frontier' = Map[Cluster, p.Set[String]]
      for (cluster, peers) in frontier.pairs() do
        for peer in peers.values() do
          let cluster' = cluster + peer
          if cluster' == cluster then continue end
          let peers' = peers and edges_for(peer)
          frontier'(cluster') = peers'
        end
      end
      frontier = frontier'
      count = count + 1
    end
    for cluster in frontier.keys() do
      return cluster
    end
    error

  fun edges_for(node: String): p.Set[String] =>
    try edges(node)? else p.Set[String] end

actor Day23
  be part1(input: Array[String] val, out: OutStream) =>
    let graph = parse_input(input)
    var result: U64 = 0
    for cluster in graph.clusters_of_three().values() do
      var has_t = false
      for node in cluster.nodes.values() do
        if node.at("t", 0) then has_t = true end
      end
      if has_t then
        result = result + 1
      end
    end
    out.print("part 1: " + result.string())

  be part2(input: Array[String] val, out: OutStream) =>
    let graph = parse_input(input)
    try
      let largest_cluster = graph.largest_cluster()?
      let result = ",".join(largest_cluster.nodes.values())
      out.print("part 2: " + result.clone())
    else
      out.print("part 2: unsolvable")
    end

  fun parse_input(input: Array[String] val): Graph =>
    let nodes = Set[String]
    let edges = Map[String, p.Set[String]]
    for line in input.values() do
      (let from, let to) = line.clone().chop(2)
      let from' = recover val from.clone() end
      let to' = recover val to.substring(1) end
      nodes.set(from')
      nodes.set(to')

      let from_edges = edges.get_or_else(from', p.Set[String])
      edges(from') = from_edges + to'
      let to_edges = edges.get_or_else(to', p.Set[String])
      edges(to') = to_edges + from'
    end
    Graph(nodes, edges)
  
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

    Day23.part1(input', env.out)
    Day23.part2(input', env.out)