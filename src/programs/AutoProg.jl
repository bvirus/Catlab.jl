module AutoProg
export @autoprog, autoparse_wiring_diagram, parallel_schedule, generate_plan

using GeneralizedGenerated: mk_function
using MLStyle: @match

using ...Catlab
using ...Catlab.Core
using ...Catlab.CategoricalAlgebra
import ...Meta: Expr0
using ...Theories: ObExpr, HomExpr, otimes, munit, dom, codom
using ...WiringDiagrams
using ...WiringDiagrams.DirectedWiringDiagrams
using ..GenerateJuliaPrograms: make_return_value

""" Parse a wiring diagram from a Julia program.

For the most part, this is standard Julia code but a few liberties are taken
with the syntax. Products are represented as tuples. So if `x` and `y` are
variables of type ``X`` and ``Y``, then `(x,y)` has type ``X ⊗ Y``. Also, both
`()` and `nothing` are interpreted as the monoidal unit ``I``.

Unlike standard Julia, the function call expressions `f(x,y)` and `f((x,y))` are
equivalent. Consequently, given morphisms ``f: W → X ⊗ Y`` and ``g: X ⊗ Y → Z``,
the code

```julia
x, y = f(w)
g(x,y)
```

is equivalent to `g(f(w))`. In standard Julia, at most one of these calls to `g`
would be valid, unless `g` had multiple signatures.

The diagonals (copying and deleting) of a cartesian category are implicit in the
Julia syntax: copying is variable reuse and deleting is variable non-use. For
the codiagonals (merging and creating), a special syntax is provided,
reinterpreting Julia's vector literals. The merging of `x1` and `x2` is
represented by the vector `[x1,x2]` and creation by the empty vector `[]`. For
example, `f([x1,x2])` translates to `compose(mmerge(X),f)`.

This macro is a wrapper around [`parse_wiring_diagram`](@ref).
"""
macro autoprog(syntax, exprs...)
  Expr(:call, GlobalRef(AutoProg, :autoparse_wiring_diagram),
    esc(syntax), (QuoteNode(expr) for expr in exprs)...)
end

struct FakeHom
  name :: Symbol
  in_sig :: Vector{Symbol}
  out_sig :: Vector{Symbol}
end

function Base.:(==)(left :: FakeHom, right :: FakeHom) 
  return left.name == right.name && left.in_sig == right.in_sig && left.out_sig == right.in_sig
end

mutable struct FakePres
  obs :: Set{Symbol}
  homs :: Dict{Symbol, FakeHom }
end

""" Parse a wiring diagram from a Julia function expression.

For more information, see the corresponding macro [`@program`](@ref).
"""
# This function converts @autoprog(syntax, () -> ...) or @autoprog(syntax,
# function() end) calls into the expected format that occurs when you use
# @autoprog (args) begin end
function autoparse_wiring_diagram(syntax::Module, expr::Expr)::Tuple{WiringDiagram, Presentation}
  @match expr begin
    Expr(:function, call, body) => autoparse_wiring_diagram(syntax, call, body)
    Expr(:->, call, body) => autoparse_wiring_diagram(syntax, call, body)
    _ => error("Not a function or lambda expression")
  end
end

# The main branch of autoparse
function autoparse_wiring_diagram(syntax_module::Module, call::Expr0, body::Expr)::Tuple{WiringDiagram, Presentation}
  # Parse argument names and types from call expression.
  call_args = @match call begin
    Expr(:call, name, args...) => args
    Expr(:tuple, args...) => args
    Expr(:(::), _...) => [call]
    _::Symbol => [call]
    _ => error("Invalid function signature: $call")
  end
  parsed_args = map(call_args) do arg
    @match arg begin
      Expr(:(::), name::Symbol, type_expr::Symbol) => (name, type_expr)
      _ => error("Argument $arg is missing name or type")
    end
  end

  # Compile...
  args = Symbol[ first(arg) for arg in parsed_args ]
  # kwargs = make_lookup_table(pres, syntax_module, unique_symbols(body))
  func_expr = compile_recording_expr(body, args)
  func = mk_function(parentmodule(syntax_module), func_expr)

  pres = Presentation(syntax_module)

  # ...and then evaluate function that records the function calls.
  arg_obs = [ invoke_term(syntax_module, :Ob, arg) for arg in unique(map(last, parsed_args)) ]
  for arg in arg_obs
    add_generator!(pres, arg)
  end
  
  arg_blocks = Int[ length(to_wiring_diagram(ob)) for ob in arg_obs ]
  inputs = to_wiring_diagram(otimes(arg_obs))
  diagram = WiringDiagram(inputs, munit(typeof(inputs)))
  v_in, v_out = input_id(diagram), output_id(diagram)
  arg_ports = [ Tuple(Port(v_in, OutputPort, i) for i in (stop-len+1):stop)
                for (len, stop) in zip(arg_blocks, cumsum(arg_blocks)) ]
  recorder = (args...) -> record_call!(diagram, pres, args...)
  value = func(recorder, arg_ports...)

  # Add outgoing wires for return values.
  out_ports = normalize_arguments((value,))
  add_output_ports!(diagram, [
    # XXX: Inferring the output port types is not reliable.
    port_value(diagram, first(ports)) for ports in out_ports
  ])
  add_wires!(diagram, [
    port => Port(v_out, InputPort, i)
    for (i, ports) in enumerate(out_ports) for port in ports
  ])
  return substitute(diagram), pres
end

""" Make a lookup table assigning names to generators or term constructors.
"""
function make_lookup_table(pres::FakePres, syntax_module::Module, names)
  theory = GAT.theory(syntax_module.theory())
  terms = Set([ term.name for term in theory.terms ])

  table = Dict{Symbol,Any}()
  for name in names
    if has_generator(pres, name)
      table[name] = generator(pres, name)
    elseif name in terms
      table[name] = (args...) -> invoke_term(syntax_module, name, args...)
    end
  end
  table
end

""" Evaluate pseudo-Julia type expression, such as `X` or `otimes{X,Y}`.
"""
function eval_type_expr(syntax_module :: Module, expr::Expr0)
  @match expr begin
    Expr(:curly, name, args...) => begin
      invoke_term(syntax_module, name, map(_eval_type_expr, args)...)
    end
    name::Symbol => begin 
      
    end
    _ => error("Invalid type expression $expr")
  end
end

""" Generate a Julia function expression that will record function calls.

Rewrites the function body so that:

  1. Ordinary function calls are mapped to recorded calls, e.g.,
     `f(x,y)` becomes `recorder(f,x,y)`
  2. "Curly" function calls are mapped to ordinary function calls, e.g.,
     `f{X,Y}` becomes `f(X,Y)`
"""
function compile_recording_expr(body::Expr, args::Vector{Symbol};
    kwargs::Vector{Symbol}=Symbol[],
    recorder::Symbol=Symbol("##recorder"))::Expr
  function rewrite(expr)
    @match expr begin
      Expr(:(::), Expr(:call, f, args...), rettype::Symbol) => begin
        rettype = QuoteNode(rettype) # TODO use syntax module
        return Expr(:call, recorder, QuoteNode(f), :([ $rettype ]),  map(rewrite, args)...)
      end
      Expr(:(::), Expr(:call, f, args...), types) => begin
        types = map(QuoteNode, types.args) # TODO use syntax module
        return Expr(:call, recorder, QuoteNode(f), :([ $(types...) ]), map(rewrite, args)...)
      end
      Expr(:call, f, args...) =>
        Expr(:call, recorder, QuoteNode(f), map(rewrite, args)...)
      Expr(:curly, f, args...) =>
        Expr(:call, rewrite(f), map(rewrite, args)...)
      Expr(head, args...) => Expr(head, map(rewrite, args)...)
      _ => expr
    end
  end
  Expr(:function,
    Expr(:tuple,
      Expr(:parameters, (Expr(:kw, kw, nothing) for kw in kwargs)...),
      recorder, args...),
    rewrite(body))
end

""" Record a Julia function call as a box in a wiring diagram.
"""
function record_call!(diagram::WiringDiagram, pres::Presentation, f::Symbol, args...)
  record_call!(diagram, pres, f, nothing, args...) 
end
function record_call!(diagram::WiringDiagram, pres::Presentation, f::Symbol, out_sig::Union{Vector{Symbol}, Nothing}, args...)
  # Add a new box, itself a wiring diagram, for the call.
  args = [ isa(a, Port) ? (a,) : a for a in args ]
  term_args = [ port_value(diagram, first(arg)) for arg in args ]

  g = if has_generator(pres, f)
    g = generator(pres, f)
    @assert gat_typeof(g) == :Hom
    @assert dom(pres[f]) == foldl(otimes, [ pres[a] for a in term_args ] )
    if out_sig != nothing
      @assert codom(pres[f]) == foldl(otimes, [ pres[o] for o in out_sig ])
    end
    g
  else 
    @assert out_sig != nothing
    term_outs = invoke_term(pres.syntax, :munit)
    for o in out_sig
      term = if has_generator(pres, o)
        pres[o]
      else 
        t = invoke_term(pres.syntax, :Ob, o)
        add_generator!(pres, t)
      end
      term_outs = otimes(term_outs, term)
    end
    term = invoke_term(pres.syntax, :Hom, f, foldl(otimes, [ pres[a] for a in term_args ]), term_outs)
    add_generator!(pres, term)
  end

  subdiagram = to_wiring_diagram(g)
  v = add_box!(diagram, subdiagram)

  # Adding incoming wires.
  inputs = input_ports(subdiagram)
  arg_ports = normalize_arguments(Tuple(args))
  @assert length(arg_ports) == length(inputs)
  add_wires!(diagram, [
    Wire(port => Port(v, InputPort, i))
    for (i, ports) in enumerate(arg_ports) for port in ports
  ])

  # Return output ports.
  outputs = output_ports(subdiagram)

  return_ports = [ Port(v, OutputPort, i) for i in eachindex(outputs) ]
  make_return_value(return_ports)
end

""" Normalize arguments given as (possibly nested) tuples or vectors of values.
"""
function normalize_arguments(xs::Tuple)
  mapreduce(normalize_arguments, (xs,ys) -> (xs..., ys...), xs; init=())
end
function normalize_arguments(xs::Vector)
  xss = map(normalize_arguments, flatten_vec(xs)) # Vector of lists of vectors
  if isempty(xss)
    ([],) # Degenerate case
  else
    Tuple(reduce(vcat, xs) for xs in zip(xss...))
  end
end
normalize_arguments(::Nothing) = ()
normalize_arguments(x) = ([x],)
flatten_vec(xs::Vector) = mapreduce(flatten_vec, vcat, xs; init=[])
flatten_vec(x) = [x]

""" Set of all symbols occuring in a Julia expression.
"""
unique_symbols(expr::Expr) =
  reduce(union!, map(unique_symbols, expr.args); init=Set{Symbol}())
unique_symbols(x::Symbol) = Set([x])
unique_symbols(x) = Set{Symbol}()


function parallel_schedule(graph::ACSet)
  # would be nice to implement this directly without
  # cheating but its easier to just use the built-in
  # topological_sort
  sorted = topological_sort(graph)

  level = 0
  stages = []
  stage = []

  # this code is adapted an internal catlab method
  # for finding the depths of all nodes in a graph
  lengths = fill(0, nv(graph))
  for v in sorted
      lengths[v] = mapreduce(max, inneighbors(graph, v), init = 0) do u
          lengths[u] + 1
      end
      if lengths[v] > level
          push!(stages, stage)
          stage = [v]
          level = lengths[v]
      else
          push!(stage, v)
      end
  end

  push!(stages, stage)
  return stages
end


## A plan is a vector of vectors of strings, each string
## being a command to execute on the command line.
## Right now, there's no support for custom names
## so the final output is just the output file with the 
## largest number in the name
function generate_plan(prog::WiringDiagram, ops::Dict{Symbol,Function}, inputs::Vector)::Vector{Vector{String}}
  id = 0

  # generate the command line string and the names
  # of the output files give a task and a list of files
  function apply(opcode :: Symbol, inputs::Vector)
      return ops[opcode](inputs...)
  end

  d = prog.diagram
  graph = internal_graph(prog)
  @assert length(inputs) == nparts(d, :OuterInPort)
  invals = Dict()
  outvals = Dict()
  outputs = Dict()


  for iw in parts(d, :InWire)   # initialize input ports with the arguments to the function
      v = inputs[d[iw, :in_src]]
      invals[d[iw, :in_tgt]] = v
  end

  # the schedule is the outline of our final plan
  # just with the node numbers instead of the correct
  # strings
  schedule = parallel_schedule(graph)

  # so we can map over the plan and just transform
  # everything in place (it's in the correct order
  # thanks to using topological_sort)
  println(schedule)
  plan = map(schedule) do stage
      return map(stage) do item

          # get the task instance and all the input
          # Files from the appropriate wires
          opcode = d[item, :value]
          inslots = incident(d, item, :in_port_box)
          args = map(inslots) do slot
              return invals[slot]
          end

          # generate the command string
          cmd, rets = apply(opcode, args)
          outslots = incident(d, item, :out_port_box)
          println((cmd, rets, outslots))
          @assert length(rets) == length(outslots)
          for (i, slot) in pairs(outslots) # set all the output ports
              outvals[slot] = rets[i]
          end
          # synchronize via the wires (pass output port data to input ports)
          println("invals ", invals, " ", rets, " ", outvals)
          println(incident(d, item, [:src]), " ", incident(d, item, [:out_port_box]))
          for w in incident(d, item, [:src, :out_port_box])
            tgt = d[w, :tgt]
            src = d[w, :src]
            println(tgt, " ", src)
            invals[tgt] = outvals[src]
          end
          return cmd
      end
  end
  # TODO: remove all .o files command
  # while i > 0
  #     push!(plan, ["rm"])
  return plan
end

end