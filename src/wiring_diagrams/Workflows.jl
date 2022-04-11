module Workflows
export ShellTask, depths, traverse_diagram, @shelltask, compile_to_shell, compile_to_makefile

using ...Catlab
using ...Catlab.Core
using ...Catlab.CategoricalAlgebra
using ...WiringDiagrams
using ...WiringDiagrams.DirectedWiringDiagrams


struct ShellTask 
  construct :: Function
  input_size :: Int64
  output_size :: Int64
end

function depths(graph::ACSet)
  # would be nice to implement this directly without
  # cheating but its easier to just use the built-in
  # topological_sort
  sorted = topological_sort(graph)

  # this code is adapted an internal catlab method
  # for finding the depths of all nodes in a graph
  lengths = fill(0, nv(graph))
  for v in sorted
      lengths[v] = mapreduce(max, inneighbors(graph, v), init = 0) do u
          lengths[u] + 1
      end
  end

  return pairs(lengths) |> Dict
end


function traverse_diagram(visit, inputs :: Vector, prog::WiringDiagram) 
  d = prog.diagram
  graph = internal_graph(prog)
  @assert length(inputs) == nparts(d, :OuterInPort)
  invals = Dict()
  outvals = Dict()

  # initialize input ports with the arguments to the function
  # maps from the input wire's target to the correct value from inputs
  for iw in parts(d, :InWire)   
      v = inputs[d[iw, :in_src]]
      invals[d[iw, :in_tgt]] = v
  end

  sorted = topological_sort(graph)

  for node in sorted
    inslots = incident(d, node, :in_port_box)
    args = [ invals[slot] for slot in inslots ]

    # visit the node and get outputs to track
    rets = visit(node, args)

    # make sure the visitor returned the right number of outputs
    outslots = incident(d, node, :out_port_box)
    @assert length(rets) == length(outslots)

    # add new outvals to the dictionary
    merge!(outvals, zip(outslots, rets) |> Dict)

    # synchronize via the wires (pass output port data to input ports)
    for w in incident(d, node, [:src, :out_port_box])
      tgt = d[w, :tgt]
      src = d[w, :src]
      invals[tgt] = outvals[src]
    end
  end

  # return the outvals that correspond to the overall outputs of the program
  return [ outvals[k] for k in subpart(d, parts(d, :OutWire), :out_src) ]
end


macro shelltask(inputs...)
  input_size = 0
  output_size = 0

  transform(input :: Symbol) = esc(input)
  transform(input :: String) = input
  function transform(item :: Expr)
    if item.args[1] == :input
      input_size += 1
      return item
    elseif item.args[1] == :output
      output_size += 1
      return item
    else
      return esc(item)
    end
  end
  parts = [ transform(part) for part in inputs ]
  return quote
    ShellTask(
      function(input, output)
        return join([$( parts... ) ], " ")
      end,
      $(input_size),
      $(output_size)
    )
  end
end

function compile_to_shell(mapping :: Dict{Symbol, ShellTask}, (inputs, outputs) :: Pair{Vector{String}, Vector{String}}, diagram :: WiringDiagram)
  return compile_to_shell(mapping, inputs, outputs, diagram)
end
function compile_to_shell(mapping :: Dict{Symbol, ShellTask}, inputs :: Vector{String}, outputs :: Vector{String}, diagram :: WiringDiagram)
  d = diagram.diagram
  removes = Set{String}()
  script = []
  id = 0

  # TODO find a way to reorder to the outputs based on index

  outs = traverse_diagram(inputs, diagram) do box :: Int64, deps :: Vector{String}
    label = d[box, :value]
    outs = Dict()
    function newoutput(pos, ext = "")
      id += 1
      v = "out$id$ext"
      push!(removes, "rm $v") 
      outs[pos] = v
      return v
    end
  
    push!(script, mapping[label].construct(i -> deps[i], newoutput))
    return sort(collect(outs), by=first) |> y -> map(last, y)
  end

  push!(script, [ "mv $left $right" for (left, right) in zip(outs, outputs) ]...)
  push!(script, removes...)
  return join(script, "\n")
end

function compile_to_makefile(mapping :: Dict{Symbol, ShellTask}, (inputs, outputs) :: Pair{Vector{String}, Vector{String}}, diagram :: WiringDiagram)
  return compile_to_makefile(mapping, inputs, outputs, diagram)
end
function compile_to_makefile(mapping :: Dict{Symbol, ShellTask}, inputs :: Vector{String}, outputs :: Vector{String}, diagram :: WiringDiagram)
  script = []
  removes = Set{String}()

  id = 0

  d = diagram.diagram
  outs = traverse_diagram(inputs, diagram) do box :: Int64, deps :: Vector{String}
    label = d[box, :value]
    outs = Dict()
    function newoutput(pos, ext = "")
      id += 1
      v = "out$id$ext"
      push!(removes, "rm $v") 
      outs[pos] = v
      return v
    end
    str = mapping[label].construct(i -> deps[i], newoutput)
    str = replace(str, "\n" => "\n\t")
    
    outs = sort(collect(outs), by=first) |> y -> map(last, y) 
    push!(script, "$(join(outs, " ")) : $(join(deps, " "))\n\t$str")

    return outs
  end
  moves = join([ "mv $left $right" for (left, right) in zip(outs, outputs) ], "\n\t")
  push!(script, ".PHONY: all\nall: $(join(outs, " "))\n\t$moves " )
  push!(script, ".PHONY: clean\nclean:\n\t$(join(removes, "\n\t"))")
  return join(script, "\n\n")
end


end