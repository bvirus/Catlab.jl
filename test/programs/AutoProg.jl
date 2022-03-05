module TestAutoProg
using Test
using Catlab, Catlab.Theories, Catlab.WiringDiagrams

using Catlab.Programs
using Catlab.Programs.AutoProg

test = @autoprog FreeMonoidalCategory(a::A, b::B)
    return join(a, b)::C
end



end