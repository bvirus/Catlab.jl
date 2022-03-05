using Test

@testset "AutoProg" begin
  include("AutoProg.jl")
end

@testset "GenerateJuliaPrograms" begin
  # include("GenerateJuliaPrograms.jl")
end

@testset "ParseJuliaPrograms" begin
  # include("ParseJuliaPrograms.jl")
end

@testset "RelationalPrograms" begin
  # include("RelationalPrograms.jl")
end

@testset "DiagrammaticPrograms" begin
  # include("DiagrammaticPrograms.jl")
end
