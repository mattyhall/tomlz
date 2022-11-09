# tomlz
A TOML parser for Zig targeting TOML v1.0.0, an easy API and safety.

```zig
  var gpa = std.heap.page_allocator;
  var table = try parser.parse(gpa,
      \\foo = 1
      \\bar = 2
 
  );
  defer table.deinit(gpa);
  
  table.get("foo").?.integer; // 1
```

## Installation
TODO

## Usage
TODO

## Goals and non-goals
Goals and non-goals are subject to change based on how the project is used and
my own time constraints. If you feel a goal or non-goal isn't quite right please
open an issue and we can discuss it.

### Goals
- TOML v1.0.0. The datetime portion of this is probably going to be
  unachievable until Zig gets a good standard library type for it or a library
  gets dominance. Other than that however we should pass all the
  [tests](https://github.com/BurntSushi/toml-test)
- A nice API. Getting values from the `Value` type should be painless as
  possible and we should also provide deserialising a `Table` into a struct,
  similarly to how `std.json` does it
- Easy installation. We should try to make using the library as a vendored
  dependency and as a package - on e.g. [gyro](https://github.com/mattnite/gyro)
  \- as easy as possible
- Safety. The parser should never crash no matter the input. To achieve this we
  should run fuzzing against it
- Support Zig master and the latest tagged release until Zig v1.0. This will be
  done by having the main branch track Zig master and a branch for each Zig
  release. Any improvements should be backported to the most recent release
  branch
- Good error messages

### Non-goals
- Super duper performance. We want to be as performant as possible without
  making the code harder to read. It is unlikely that parsing a TOML file is
  going to be the bottleneck in your application so "good" performance should be
  sufficient
- Previous versions of TOML