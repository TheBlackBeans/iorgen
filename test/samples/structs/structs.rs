use std::io;

/// A simple struct
struct Struct1 {
    /// a field
    foo: i32,
    /// a field
    bar: i32,
}

/// Represents a position
struct Position {
    /// X
    x: i32,
    /// Y
    y: i32,
    /// Z
    z: i32,
}

/// A point's name and position
struct Point {
    /// the point's name (single character)
    name: char,
    /// the point's description
    description: String,
    /// the point's position
    pos: Position,
}

/// a struct of chars
struct Chars {
    /// a first char
    first_char: char,
    /// a second char
    second_char: char,
    /// a third char
    third_char: char,
}

/// * `struct_` - a struct 1 instance
/// * `n` - a number
/// * `struct_list` - a list a struct 1
/// * `triangle` - a triangle
/// * `struct_chars` - a struct of chars
fn structs(struct_: Struct1, n: i32, struct_list: Vec<Struct1>, triangle: Vec<Point>, struct_chars: Chars) {
    /* TODO Look at them structs. */
}

fn main() {
    let struct_: Struct1 = read_struct_struct_1();
    let n: i32 = read_line().parse().unwrap();
    let mut struct_list: Vec<Struct1> = Vec::with_capacity(n as usize);
    for _ in 0..n {
        struct_list.push(read_struct_struct_1());
    }
    let mut triangle: Vec<Point> = Vec::with_capacity(3 as usize);
    for _ in 0..3 {
        let triangle_elem: Point = read_struct_point();
        triangle.push(triangle_elem);
    }
    let struct_chars: Chars = read_struct_chars();

    structs(struct_, n, struct_list, triangle, struct_chars);
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("Failed to read line");
    line.trim().to_string()
}

fn read_struct_struct_1() -> Struct1 {
    let line = read_line();
    let words: Vec<&str> = line.split_whitespace().collect();
    Struct1 {
        foo: words[0].parse().unwrap(),
        bar: words[1].parse().unwrap(),
    }
}

fn read_struct_position() -> Position {
    let line = read_line();
    let words: Vec<&str> = line.split_whitespace().collect();
    Position {
        x: words[0].parse().unwrap(),
        y: words[1].parse().unwrap(),
        z: words[2].parse().unwrap(),
    }
}

fn read_struct_point() -> Point {
    let name: char = read_line().parse().unwrap();
    let description: String = read_line();
    let pos: Position = read_struct_position();
    Point { name, description, pos }
}

fn read_struct_chars() -> Chars {
    let line = read_line();
    let words: Vec<&str> = line.split_whitespace().collect();
    Chars {
        first_char: words[0].parse().unwrap(),
        second_char: words[1].parse().unwrap(),
        third_char: words[2].parse().unwrap(),
    }
}
