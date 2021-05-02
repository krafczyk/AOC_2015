import ArgParse

# Build ArgParsing Table
s = ArgParse.ArgParseSettings()
@ArgParse.add_arg_table s begin
    "--input"
        help = "Filepath to input data"
        arg_type = String
        required = true
    "--report"
        help = "Add extra reporting"
        action = :store_true
end

# Parse arguments
parsed_args = ArgParse.parse_args(ARGS, s)

# Get input filepath
input_filepath = parsed_args["input"]
report = parsed_args["report"]

instruction_matcher = r"(turn on|toggle|turn off) ([0-9]*),([0-9]*) through ([0-9]*),([0-9]*)"

function count_on(in::BitArray)
    return sum(in)
end

function count_on(in::Array{UInt,2})
    return sum(in)
end

function run_program_1(lines::Vector{String}, report::Bool)::Int
    # Initialize map
    lights = BitArray(undef, (1000,1000))
    lights[:,:] .= false

    if report
        num = count_on(lights)
        println("Initial on: $(num)")
    end

    for line in lines
        m = match(instruction_matcher, line)
        caps = Vector{String}(m.captures)

        if length(caps) != 5
            throw(ErrorException("Line $(line) didn't match the regex!"))
        end

        action = caps[1]
        # We need to add one because of julia indexing
        min_x = parse(Int,caps[2])+1
        min_y = parse(Int,caps[3])+1
        max_x = parse(Int,caps[4])+1
        max_y = parse(Int,caps[5])+1

        if (action === "turn off")
            if report
                println("turn off task")
            end
            lights[min_x:max_x,min_y:max_y] .= false
        elseif (action === "turn on")
            if report
                println("turn on task")
            end
            lights[min_x:max_x,min_y:max_y] .= true
        elseif (action === "toggle")
            if report
                println("toggle task")
            end
            lights[min_x:max_x,min_y:max_y] .= (!).(lights[min_x:max_x,min_y:max_y])
        else
            throw(ErrorException("Unsupported action $(action)!"))
        end

        if report
            num = count_on(lights)
            println("number on: $(num)")            
        end
    end

    num_lights_on = count_on(lights)
    return num_lights_on
end

function run_program_2(lines::Vector{String}, report::Bool)::Int
    # Initialize map
    lights = Array{UInt}(undef, (1000,1000))
    lights[:,:] .= 0

    if report
        num = count_on(lights)
        println("Initial on: $(num)")
    end

    function turn_off(x::UInt)::UInt
        if x == 0
            return x
        else
            return x-1
        end
    end

    for line in lines
        m = match(instruction_matcher, line)
        caps = Vector{String}(m.captures)

        if length(caps) != 5
            throw(ErrorException("Line $(line) didn't match the regex!"))
        end

        action = caps[1]
        # We need to add one because of julia indexing
        min_x = parse(Int,caps[2])+1
        min_y = parse(Int,caps[3])+1
        max_x = parse(Int,caps[4])+1
        max_y = parse(Int,caps[5])+1

        if (action === "turn off")
            if report
                println("turn off task")
            end
            lights[min_x:max_x,min_y:max_y] = map(turn_off, lights[min_x:max_x,min_y:max_y])
        elseif (action === "turn on")
            if report
                println("turn on task")
            end
            @. lights[min_x:max_x,min_y:max_y] = lights[min_x:max_x,min_y:max_y]+1
        elseif (action === "toggle")
            if report
                println("toggle task")
            end
            @. lights[min_x:max_x,min_y:max_y] = lights[min_x:max_x,min_y:max_y]+2
        else
            throw(ErrorException("Unsupported action $(action)!"))
        end

        if report
            num = count_on(lights)
            println("number on: $(num)")
        end
    end

    num_lights_on = count_on(lights)
    return num_lights_on
end

# Process files
open(input_filepath) do f
    lines = collect(readlines(f))

    num_lights_on_1 = run_program_1(lines, report)
    println("Task 1: Number of lights on: $(num_lights_on_1)")

    num_lights_on_2 = run_program_2(lines, report)
    println("Task 2: Number of lights on: $(num_lights_on_2)")
end
