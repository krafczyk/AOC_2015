import ArgParse
import MD5

# Build ArgParsing Table
s = ArgParse.ArgParseSettings()
@ArgParse.add_arg_table s begin
    "--input"
        help = "input_string"
        arg_type = String
end

# Parse arguments
parsed_args = ArgParse.parse_args(ARGS, s)

# Get input filepath
input_string = parsed_args["input"]

function advent_hash(key::String, n::T) where {T<:Integer}
    string_to_hash = key*"$(n)"
    return MD5.md5(string_to_hash)
end

function find_first_hash(::Type{T}, key::String, num_zeros::Int=5)::T where {T<:Integer}
    num_zero_bytes = div(num_zeros , 2)
    zeros_array = zeros(UInt8, num_zero_bytes)
    byte_mask = 0xF0
    n::T = 1
    while true
        adhash = advent_hash(key, n)
        if rem(num_zeros , 2) != 0
            # One remainder byte we need to check
            if (adhash[1:num_zero_bytes] == zeros_array) && (adhash[num_zero_bytes+1] & byte_mask == 0)
                break
            end
        else
            # No remainder bytes
            if adhash[1:num_zero_bytes] == zeros_array
                break
            end
        end
        n += 1
        # Check for overflow
        if typeof(n) in [Int32, Int64]
            if n == typemax(typeof(n))
                throw(ErrorException("Iterated beyond max capacity of $(typeof(n))"))
                return n
            end
        end
    end
    return n
end

n = find_first_hash(Int, input_string)

println("Task 1: First solved when n == $(n)")

n = find_first_hash(Int, input_string, 6)

println("Task 2: First solved when n == $(n)")
