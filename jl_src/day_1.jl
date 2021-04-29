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

# Process files
open(input_filepath) do f
    for line in readlines(f)
        floor = 0
        first_basement = nothing
        for i in 1:length(line)
            if line[i] == '('
                floor += 1
            elseif line[i] == ')'
                floor -= 1
            else
                throw(ErrorException("Character $(line[i]) not supported"))
            end
            if floor < 0 && isnothing(first_basement)
                first_basement = i
            end
        end
        println("Line length: $(length(line)) Task 1: $(floor) Task 2: $(first_basement)")
        print
    end
end
