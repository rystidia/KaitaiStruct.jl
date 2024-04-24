# Kaitai Struct: runtime library for Julia

This library implements Kaitai Struct API for Julia.

[![Build status (Github Actions)](https://github.com/rystidia/KaitaiStruct.jl/workflows/CI/badge.svg)](https://github.com/rystidia/KaitaiStruct.jl/actions)

Kaitai Struct is a declarative language used for describe various binary
data structures, laid out in files or in memory: i.e. binary file
formats, network stream packet formats, etc.
This library is not meant to be used by a user as a standalone project. It is designed to be a part of Kaitai Struct project.
For example, the following specification defines a string preceded by an integer that designates its length:
```yaml
seq:
  - id: my_len
    type: u4
  - id: my_str
    type: str
    size: my_len
    encoding: UTF-8
```
With this library, the described data can be parsed in the following way:
```julia
my_len = KaitaiStruct.readU4(io)
my_str = decode(KaitaiStruct.read_bytes(io, my_len), "UTF-8")
```

Further reading:

* [About Kaitai Struct](http://kaitai.io/)
* [About API implemented in this library](http://doc.kaitai.io/stream_api.html)
