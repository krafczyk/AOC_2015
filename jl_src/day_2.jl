import ArgParse

# Build ArgParsing Table
s = ArgParse.ArgParseSettings()
@ArgParse.add_arg_table s begin
    "--input"
        help = "Filepath to input data"
        arg_type = String
end

# Parse arguments
parsed_args = ArgParse.parse_args(ARGS, s)

# Get input filepath
input_filepath = parsed_args["input"]

"""
Struct to store box dimensions
"""
struct BoxDims
    l::Int
    w::Int
    h::Int
end

"""
Function to build box from string
"""
function BoxDims(s::String)::BoxDims
    re = r"([0-9]*)x([0-9]*)x([0-9]*)"
    m = match(re, s)
    if m === nothing
        throw(ErrorException("String $(s) is of incorrect format."))
        return BoxDims(0,0,0)
    else
        return BoxDims(map(s -> parse(Int, s),m.captures)...)
    end
end

function NeededPaper(b::BoxDims)::Int
    areas = [b.l*b.w, b.w*b.h, b.l*b.h]
    min_area = minimum(areas)
    return sum(2 .* areas)+min_area
end

function NeededRibbon(b::BoxDims)::Int
    ribbon = b.l*b.w*b.h
    return sum(2 .* sort([b.l, b.w, b.h])[1:2])+ribbon
end

# Process files
boxes = Vector{BoxDims}()
open(input_filepath) do f
    for line in readlines(f)
        push!(boxes, BoxDims(line))
    end
end

needed_paper = sum(map(NeededPaper, boxes))
needed_ribbon = sum(map(NeededRibbon, boxes))

println("Task 1 - Needed paper: $(needed_paper)")
println("Task 2 - Needed ribbon: $(needed_ribbon)")
