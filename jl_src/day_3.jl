import ArgParse
import Base

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

struct Point{T<:Number}
    x::T
    y::T
end

Point(x::T) where {T<:Number} = Point(x,x)
Point(x::T, y::T) where {T<:Number} = Point{T}(x, y)

Base.:+(a::Point{T}, b::Point{T}) where {T<:Number} = Point(a.x+b.x, a.y+b.y)

dir_dict = Dict{Char,Point}([
    ('^', Point{Int}(0, 1)),
    ('v', Point{Int}(0, -1)),
    ('<', Point{Int}(-1, 0)),
    ('>', Point{Int}(1, 0))])

# Define map type
Map = Dict{Point,Int}

# Process files
open(input_filepath) do f
    for line in readlines(f)
        # Cast line
        vec_line = Vector{Char}(line)
        # Solve Task 1
        cur_pos = Point(0)
        map = Map([(cur_pos, 1)])
        for c in vec_line
            dir = dir_dict[c]
            cur_pos += dir
            map[cur_pos] = get(map, cur_pos, 0)+1
        end
        houses_visited = length(map)
        println("Task 1: $(houses_visited) houses visited")
        # Solve Task 2
        ids = collect(1:length(line))
        even_pos = (ids .% 2 .== 0)
        odd_pos = (ids .%2 .== 1)
        map = Map()
        cur_pos = Point(0)
        map[cur_pos] = get(map, cur_pos, 0)+1
        for c in vec_line[even_pos]
            dir = dir_dict[c]
            cur_pos += dir
            map[cur_pos] = get(map, cur_pos, 0)+1
        end
        cur_pos = Point(0)
        for c in vec_line[odd_pos]
            dir = dir_dict[c]
            cur_pos += dir
            map[cur_pos] = get(map, cur_pos, 0)+1
        end
        houses_visited = length(map)
        println("Task 2: $(houses_visited) houses visited")
    end
end
