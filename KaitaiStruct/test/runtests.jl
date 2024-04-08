using KaitaiStruct
using Test

@testset "KaitaiStream tests" begin

    @testset "readS1 test" begin
        stream = KaitaiStruct.KaitaiStream(IOBuffer(b"12345"))
        first = KaitaiStruct.readS1(stream)
        @test first == 0x31
        second = KaitaiStruct.readS1(stream)
        @test second == 0x32
    end

    @testset "readS2be test" begin
        stream = KaitaiStruct.KaitaiStream(IOBuffer(b"12345"))
        first = KaitaiStruct.readS2be(stream)
        @test first == 0x3132
        second = KaitaiStruct.readS2be(stream)
        @test second == 0x3334

        @test_throws ErrorException KaitaiStruct.readS2be(stream)
    end

    @testset "read_bytes5 test" begin
        stream = KaitaiStruct.KaitaiStream(IOBuffer(b"12345"))
        actual = KaitaiStruct.read_bytes(stream, 5)

        @test actual == b"12345"
    end

    @testset "read_bytes6 test" begin
        stream = KaitaiStruct.KaitaiStream(IOBuffer(b"12345"))
        @test_throws ErrorException KaitaiStruct.read_bytes(stream, 6)
    end
end
