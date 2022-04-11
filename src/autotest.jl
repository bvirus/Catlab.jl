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
using Catlab.WiringDiagrams.Workflows
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
    c = f(a, b)::A
    c2 = f(c, b)
    d, e = g(c, c2)::(D, E)
    return h(c, d, e)::F
end

# to draw arbitrary presentation, look at AlgebraicPetri, Theory of Petri Nets.
# Ask andrew and micah (Github Issue), how to convert SMC presentation to a
# Petri Net
# show(pres)
param = "--option"

# think about $1 for input and &2> for outputs
# less than for inputs and greater than for outputs
defs = Dict(
  :f => (@shelltask "execute-f" param input(1) input(2) "-o" output(1, ".txt")),
  :g => (@shelltask "execute-g" input(1) input(2) "-o" output(1, ".txt") output(2, ".txt")),
  :h => (@shelltask "execute-h" input(1) input(2) input(3) "-o" output(1, ".txt"))
)

shell_script = compile_to_shell(defs, ["a.txt", "b.txt"] => ["final.txt"], test)

# make phony targets (low priority)
# make some real world examples
makefile = compile_to_makefile(defs, ["a.txt", "b.txt"] => ["final.txt"], test)

print("Done!")

# sdl_test, sdl_test_pres = @autoprog FreeSymmetricMonoidalCategory (file1::CPPFILE, file2::CPPFILE, file3::CPPFILE, file4::CPPFILE) begin
#     main = SDL_GPP(file1)::ObjectFile
#     screen = SDL_GPP(file2)
#     particle = GPP(file3)::ObjectFile
#     swarm = GPP(file4)
#     particleSwarm = LINK_CPP(particle, swarm)::ObjectFile
#     screenPS = LINK_CPP(screen, particleSwarm)
#     allLinked = LINK_CPP(main, screenPS)
#     exe = MAKE_SDL_EXE(allLinked)::ExeFile
#     return exe
# end

# sdl_dict = Dict(
#   :GPP => (@shelltask "g++" input(1) "-c" "-o" output(1))
#   :SDL_GPP => (@shelltask "g++" input(1) "-c" "-o" output(1)).
#   :LINK_CPP => (@shelltask "ld -relocatable" input(1) input(2) "-o" output(1, ext=".o"))
#   :MAKE_SDL_EXE => (@shelltask "g++" input(1) "-o" output(1) "\$(sdl2-config --libs)")
# )


# look into functor function and tyler's blog post for mapping WiringDiagram
# into semantics. Start with julia functions, move into Makefiles/WDL. Parallel
# schedule oapply map. to_hom_expr function. PEG.jl 