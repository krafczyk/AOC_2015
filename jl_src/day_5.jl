import ArgParse

# Build ArgParsing Table
s = ArgParse.ArgParseSettings()
@ArgParse.add_arg_table s begin
    "--input"
        help = "Filepath to input data"
        arg_type = String
        required = true
    "--report"
        help = "Whether to produce a detailed report"
        action = :store_true
end

# Parse arguments
parsed_args = ArgParse.parse_args(ARGS, s)

# Get input filepath
input_filepath = parsed_args["input"]
report = parsed_args["report"]

function is_nice_1(s::String, report::Bool=false)::Bool
    if report
        println("Analyzing string $(s)")
    end
    vec_s = Vector{Char}(s)
    vowels = Char['a', 'e', 'i', 'o', 'u']
    diff = vec_s[1:length(vec_s)-1]-vec_s[2:length(vec_s)]
    num_repeats = sum(diff .== 0)
    if report && (num_repeats == 0)
        println("Failed repeat test. No repeats.")
    end
    num_vowels = sum(map(x -> x in vowels, vec_s))
    if report && (num_vowels < 3)
        println("Failed num vowels test.")
    end
    bad_groups = Vector{String}(["ab", "cd", "pq", "xy"])
    num_bad = 0
    for i in 1:length(vec_s)-1
        if String(vec_s[i:i+1]) in bad_groups
            num_bad += 1
        end
    end
    if report && (num_bad > 0)
        println("Failed bad string test.")
    end
    return (num_repeats > 0) && (num_vowels >= 3) && (num_bad == 0)
end

function is_nice_2(s::String, report::Bool=false)::Bool
    if report
        println("Analyzing string $(s)")
    end
    vec_s = Vector{Char}(s)
    num_spaced = 0
    for i in 1:length(vec_s)-2
        if vec_s[i] == vec_s[i+2]
            num_spaced += 1
        end
    end
    if report && (num_spaced == 0)
        println("Failed spaced test.")
    end
    # Build list of all unique 2char pairs
    char_pairs = Vector{Vector{Char}}()
    unique_char_pairs = Vector{Vector{Char}}()
    for i in 1:length(vec_s)-1
        char_pair = vec_s[i:i+1]
        push!(char_pairs, char_pair)
        if (!(char_pair in unique_char_pairs))
            push!(unique_char_pairs, char_pair)
        end
    end
    if report
        println("Number of unique char pairs: $(length(char_pairs))")
        @show char_pairs
        @show unique_char_pairs
    end
    found_pair = false
    for char_pair in unique_char_pairs
        # build map of matching pairs
        matching_pairs = collect(map(p -> p == char_pair, char_pairs))
        if report
            println("looking at pair $(char_pair)")
            @show matching_pairs
        end
        # Count whether there's two occurances in the pairs
        for i in 1:length(matching_pairs)-2
            if matching_pairs[i]
                if sum(matching_pairs[i+2:length(matching_pairs)]) > 0
                    found_pair = true
                    break
                end
            end
        end
        if found_pair
            break
        end
    end
    if report
        println("Final report: num_spaced: $(num_spaced) found pair: $(found_pair)")
    end
    return (num_spaced > 0) && found_pair
end

# Process files
open(input_filepath) do f
    lines = collect(readlines(f))
    if report
        println("Starting first test")
    end
    num_nice_1 = sum(map(x -> is_nice_1(x, report), lines))
    if report
        println("Starting second test")
    end
    num_nice_2 = sum(map(x -> is_nice_2(x, report), lines))
    println("Task 1: There are $(num_nice_1) nice strings.")
    println("Task 2: There are $(num_nice_2) nice strings.")
end
