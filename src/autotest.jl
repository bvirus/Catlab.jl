using Revise

using Catlab
using Catlab.Syntax: syntax_module
using Catlab.Programs, Catlab.Theories, Catlab.WiringDiagrams
using Catlab.CategoricalAlgebra
using Catlab.Programs.AutoProg
using Catlab.Core
using Catlab.Graphics
using Catlab.Graphics.Graphviz
using Catlab.WiringDiagrams
using Catlab.WiringDiagrams.DirectedWiringDiagrams

draw(d::WiringDiagram) = to_graphviz(d,
  orientation=LeftToRight,
  labels=true, label_attr=:xlabel,
  node_attrs=Graphviz.Attributes(
    :fontname => "Courier",
  ),
  edge_attrs=Graphviz.Attributes(
    :fontname => "Courier",
  )
)

draw(d) = to_graphviz(d,
  orientation=LeftToRight,
  labels=true, label_attr=:xlabel,
  node_attrs=Graphviz.Attributes(
    :fontname => "Courier",
  ),
  edge_attrs=Graphviz.Attributes(
    :fontname => "Courier",
  )
)

test, pres = @autoprog FreeSymmetricMonoidalCategory (a::A, b::B) begin
    c = f(a, b)::C
    c2 = f(a, b)
    d, e = g(c, a)::(D, E)
    return h(c, d, e)::F
end

# to draw arbitrary presentation, look at AlgebraicPetri, Theory of Petri Nets.
# Ask andrew and micah (Github Issue), how to convert SMC presentation to a
# Petri Net
show(pres)
draw(test)
typeof(FreeSymmetricMonoidalCategory)
sch = parallel_schedule(internal_graph(test))
d = test.diagram
subpart(d, [1,2], :out_port_box)

function gather(item :: Symbol, deps... :: Vector{Int})
  print(item, deps)
end
generate_plan(test, Dict(
  :f => function(x,y) return ("f", [x]) end,
  :g => function(x,y) return ("g", [x, y]) end,
  :h => function(x,y,z) return ("h", [x]) end
), ["a.txt", "b.txt"])



# look into functor function and tyler's blog post for mapping WiringDiagram
# into semantics. Start with julia functions, move into Makefiles/WDL. Parallel
# schedule oapply map. to_hom_expr function. PEG.jl 