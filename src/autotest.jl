using Revise

using Catlab
using Catlab.Programs, Catlab.Theories, Catlab.WiringDiagrams
using Catlab.Programs.AutoProg
using Catlab.Core
using Catlab.Graphics
using Catlab.Graphics.Graphviz
using Catlab.Graphics.

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

# look into functor function and tyler's blog post for mapping WiringDiagram
# into semantics. Start with julia functions, move into Makefiles/WDL. Parallel
# schedule oapply map. to_hom_expr function. PEG.jl 