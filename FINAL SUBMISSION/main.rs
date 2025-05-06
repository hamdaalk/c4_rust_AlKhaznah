use r#final::{Compiler, Opcode, Identifier, Token, Class, Type};

fn main() {
    let mut args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} [-a] <file.c>", args[0]);
        std::process::exit(1);
    }

    let mut compiler = Compiler::new();

    let libs = [
        ("open",   Opcode::OPEN),
        ("read",   Opcode::READ),
        ("close",  Opcode::CLOS),
        ("printf", Opcode::PRTF),
        ("malloc", Opcode::MALC),
        ("free",   Opcode::FREE),
        ("memset", Opcode::MSET),
        ("memcmp", Opcode::MCMP),
        ("exit",   Opcode::EXIT),
    ];

    for (name, op) in &libs {
        compiler.symbols.insert(
            name.to_string(),
            Identifier {
                token:  Token::Id,
                hash:   0,
                name:   name.to_string(),
                class:  Class::Sys,
                type_:  Type::Int,
                value:  *op as i64,
                hclass: Class::Sys,
                htype:  Type::Int,
                hval:   0,
            },
        );
    }

    let mut filename = String::new();
    for arg in &args[1..] {
        if arg == "-a" {
            compiler.print_assembly = true;
        } else {
            filename = arg.clone();
        }
    }

    if filename.is_empty() {
        eprintln!("No source file provided.");
        std::process::exit(1);
    }

    if let Err(e) = compiler.load_file(&filename) {
        eprintln!("Error reading {}: {}", filename, e);
        std::process::exit(1);
    }

    if let Err(e) = compiler.program() {
        eprintln!("Compilation error: {}", e);
        std::process::exit(1);
    }

    if compiler.print_assembly {
        compiler.print_assembly();
    } else {
        compiler.pc = compiler.symbols.get("main").unwrap().value as usize;
        compiler.run();
    }
}
