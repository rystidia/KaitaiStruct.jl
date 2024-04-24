module KaitaiStruct

export all

abstract type UserType end

mutable struct KaitaiStream
    io::IO
    bits_left
    bits
    KaitaiStream(io) = new(io, 0, 0)
end

close(stream::KaitaiStream) = close(stream.io)

pos(stream::KaitaiStream) = position(stream.io)

seek(stream::KaitaiStream, n::Integer) = Base.seek(stream.io, n)

iseof(stream::KaitaiStream) = eof(stream.io) && stream.bits_left == 0

function size(stream::KaitaiStream)
    mark(stream.io)
    seekend(stream.io)
    size = position(stream.io)
    reset(stream.io)
    size
end

readU2le(stream::KaitaiStream) = read_number_le(stream, UInt16)
readU4le(stream::KaitaiStream) = read_number_le(stream, UInt32)
readU8le(stream::KaitaiStream) = read_number_le(stream, UInt64)

readU1(stream::KaitaiStream) = read_number_be(stream, UInt8)
readU2be(stream::KaitaiStream) = read_number_be(stream, UInt16)
readU4be(stream::KaitaiStream) = read_number_be(stream, UInt32)
readU8be(stream::KaitaiStream) = read_number_be(stream, UInt64)

readS2le(stream::KaitaiStream) = read_number_le(stream, Int16)
readS4le(stream::KaitaiStream) = read_number_le(stream, Int32)
readS8le(stream::KaitaiStream) = read_number_le(stream, Int64)

readS1(stream::KaitaiStream) = read_number_be(stream, Int8)
readS2be(stream::KaitaiStream) = read_number_be(stream, Int16)
readS4be(stream::KaitaiStream) = read_number_be(stream, Int32)
readS8be(stream::KaitaiStream) = read_number_be(stream, Int64)

readF4le(stream::KaitaiStream) = read_number_le(stream, Float32)
readF8le(stream::KaitaiStream) = read_number_le(stream, Float64)
readF4be(stream::KaitaiStream) = read_number_be(stream, Float32)
readF8be(stream::KaitaiStream) = read_number_be(stream, Float64)

function read_number_le(stream::KaitaiStream, T::DataType)
    try
        return htol(read(stream.io, T))
    catch e
        if isa(e, EOFError)
            error(string("requested ", sizeof(T), " bytes, but only ", size(stream) - pos(stream), " bytes available"))
        else
            rethrow()
        end
    end
end

function read_number_be(stream::KaitaiStream, T::DataType)
    try
        return hton(read(stream.io, T))
    catch e
        if isa(e, EOFError)
            error(string("requested ", sizeof(T), " bytes, but only ", size(stream) - pos(stream), " bytes available"))
        else
            rethrow()
        end
    end
end

function read_bits_int_be(stream::KaitaiStream, n::Integer)
    res = UInt64(0)

    bits_needed = n - stream.bits_left
    stream.bits_left = -bits_needed & 7

    if bits_needed > 0
        bytes_needed::UInt64 = fld((bits_needed - 1), 8) + 1
        buf = read_bytes(stream, bytes_needed)
        for byte in buf
            res = res << 8 | (byte & 0xff)
        end

        new_bits = res
        res = res >>> stream.bits_left | stream.bits << bits_needed
        stream.bits = new_bits
    else
        res = stream.bits >>> -bits_needed
    end

    mask = (UInt64(1) << stream.bits_left) - 1
    stream.bits &= mask
    res
end

function read_bits_int_le(stream::KaitaiStream, n::Integer)
    res = UInt64(0)
    bits_needed = n - stream.bits_left

    if bits_needed > 0
        bytes_needed = ((bits_needed - 1) ÷ 8) + 1
        buf = read_bytes(stream, bytes_needed)
        for (i, byte) in enumerate(buf)
            res |= UInt64(byte) << ((i - 1) * 8)
        end

        new_bits = bits_needed < 64 ? res >>> bits_needed : 0
        res = res << stream.bits_left | stream.bits
        stream.bits = new_bits
    else
        res = stream.bits
        stream.bits >>>= n
    end

    stream.bits_left = -bits_needed & 7

    mask = (UInt64(1) << n) - 1
    res &= mask
    res
end

function read_bytes(stream::KaitaiStream, n::Integer)
    r = read(stream.io, n::Integer)
    if length(r) < n
        error(string("requested ", n, " bytes, but only ", length(r), " bytes available"))
    end
    r
end

read_bytes_full(stream::KaitaiStream) = readavailable(stream.io)

function read_bytes_term(stream::KaitaiStream, term::UInt8, include_term::Bool, consume_term::Bool, eos_error::Bool)
    out = Vector{UInt8}()
    while true
        c = eof(stream.io) ? nothing : read(stream.io, UInt8)
        if c === nothing
            eos_error && error(string("end of stream reached, but no terminator ", term, " found"))
            break
        elseif c == term
            include_term && push!(out, c)
            consume_term || seek(stream, pos(stream) - 1)
            break
        end
        push!(out, c)
    end
    out
end

function align_to_byte(stream::KaitaiStream)
    stream.bits = 0
    stream.bits_left = 0
end

function bytes_strip_right(data::Vector{UInt8}, pad_byte)
    new_len = length(data)
    while new_len >= 1 && data[new_len] == pad_byte
        new_len -= 1
    end
    data[1:new_len]
end

function bytes_terminate(src::Vector{UInt8}, term, include_term)
    new_len = 1
    max_len = length(src)

    while new_len <= max_len && src[new_len] != term
        new_len += 1
    end

    if include_term && new_len <= max_len
        new_len += 1
    end

    src[1:new_len-1]
end

function process_xor(data::Vector{UInt8}, key::UInt8)
    data .⊻ key
end

function process_xor(data::Vector{UInt8}, key::Vector{UInt8})
    data_len = length(data)
    value_len = length(key)

    r = Vector{UInt8}(undef, data_len)
    # for i in eachindex(data)
    #     r[i] = data[i] ⊻ key[i%value_len]
    # end

    j = 1
    for i in eachindex(data)
        r[i] = data[i] ⊻ key[j == 0 ? value_len : j]
        j = (j + 1) % value_len
    end
    r
end

function process_rotate_left(data::Vector{UInt8}, amount, group_size)
    group_size != 1 && error(error(string("unable to rotate group of ", group_size, " bytes yet")))

    r = Vector{UInt8}(undef, length(data))
    for i in eachindex(data)
        bits = data[i]
        r[i] = (((data[i] & 0xff) << amount) | ((bits & 0xff) >>> (8 - amount)))
    end
    r
end

function mod(a, b)
    b <= 0 && error("mod divisor <= 0")
    Base.mod(a, b)
end

function resolve_enum(enum::DataType, value)
    try
        enum(value)
    catch _
        value
    end
end

abstract type ValidationFailedError <: Exception end

struct _LocationInfo
    io::KaitaiStream
    src_path::String
end

function _msg_with_location(l::_LocationInfo, msg::String)
    string(l.src_path, ": at pos ", pos(l.io), ": ", msg)
end

function _bytes_to_hex(arr::Vector{UInt8})
    join(map(x -> string(x, base=16, pad=2), arr), " ")
end

function _format_msg_value(value::Vector{UInt8})
    string("[", _bytes_to_hex(value), "]")
end

function _format_msg_value(value::Any)
    string(value)
end

struct UndecidedEndiannessError <: Exception
    src::String
end

function Base.showerror(io::IO, e::UndecidedEndiannessError)
    print(io, "unable to decide on endianness for a type " * e.src)
end

struct ValidationNotEqualError <: ValidationFailedError
    expected::Any
    actual::Any
    location_info::_LocationInfo

    ValidationNotEqualError(expected, actual, io, src_path) =
        new(expected, actual, _LocationInfo(io, src_path))
end

function Base.showerror(io::IO, e::ValidationNotEqualError)
    print(io, _msg_with_location(e.location_info,
        "validation failed: not equal, expected $(_format_msg_value(e.expected)), but got $(_format_msg_value(e.actual))"))
end

struct ValidationLessThanError <: ValidationFailedError
    min::Any
    actual::Any
    location_info::_LocationInfo

    ValidationLessThanError(min, actual, io, src_path) =
        new(min, actual, _LocationInfo(io, src_path))
end

function Base.showerror(io::IO, e::ValidationLessThanError)
    print(io, _msg_with_location(e.location_info,
        string("validation failed: not in range, min ", _format_msg_value(e.min), ", but got ", _format_msg_value(e.actual))))
end

struct ValidationGreaterThanError <: ValidationFailedError
    max::Any
    actual::Any
    location_info::_LocationInfo

    ValidationGreaterThanError(max, actual, io, src_path) =
        new(max, actual, _LocationInfo(io, src_path))
end

function Base.showerror(io::IO, e::ValidationGreaterThanError)
    print(io, _msg_with_location(e.location_info,
        string("validation failed: not in range, max ", _format_msg_value(e.expected), ", but got ", _format_msg_value(e.actual))))
end

struct ValidationNotAnyOfError <: ValidationFailedError
    actual::Any
    location_info::_LocationInfo

    ValidationNotAnyOfError(actual, io, src_path) =
        new(actual, _LocationInfo(io, src_path))
end

function Base.showerror(io::IO, e::ValidationNotAnyOfError)
    print(io, _msg_with_location(e.location_info,
        string("validation failed: not any of the list, got ", _format_msg_value(e.expected))))
end

struct ValidationExprError <: ValidationFailedError
    actual::Any
    location_info::_LocationInfo

    ValidationExprError(actual, io, src_path) =
        new(actual, _LocationInfo(io, src_path))
end

function Base.showerror(io::IO, e::ValidationExprError)
    print(io, _msg_with_location(e.location_info,
        string("validation failed: not matching the expression, got ", _format_msg_value(e.expected))))
end

end # module KaitaiStruct
