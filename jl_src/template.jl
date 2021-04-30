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
    end
end
