use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Num(i64),
    Fun,
    Sys,
    Glo,
    Loc,
    Id(String),
    Char,
    Else,
    Enum,
    If,
    Int,
    Return,
    Sizeof,
    While,
    Assign,
    Cond,
    Lor,
    Lan,
    Or,
    Xor,
    And,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Inc,
    Dec,
    Brak,
    Eof,
}

#[derive(PartialEq)]
enum Opcode {
    LEA,
    IMM,
    JMP,
    JSR,
    BZ,
    BNZ,
    ENT,
    ADJ,
    LEV,
    LI,
    LC,
    SI,
    SC,
    PSH,
    OR,
    XOR,
    AND,
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    SHL,
    SHR,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    OPEN,
    READ,
    CLOS,
    PRTF,
    MALC,
    FREE,
    MSET,
    MCMP,
    EXIT,
}

#[derive(Debug, PartialEq, Clone)]
enum DataType {
    Char,
    Int,
    Ptr,
    Fun,
}

#[derive(Debug)]
struct Identifier {
    tk: Token,
    hash: usize,
    name: String,
    class: DataType,
    val: i64,
    is_function: bool,
}

struct Compiler {
    source: String,
    current_pos: usize,
    line: usize,
    tokens: Vec<Token>,
    symbol_table: HashMap<String, Identifier>,
    current_char: char,
    tk: Token,
    e: *mut i32,
}

impl Compiler {
    fn new(source: String) -> Self {
        Compiler {
            source,
            current_pos: 0,
            line: 1,
            tokens: Vec::new(),
            symbol_table: HashMap::new(),
            current_char: '\0',
            tk: Token::Eof,
            e: std::ptr::null_mut(),
        }
    }

    fn next(&mut self) {
        let mut pp: usize;

        while self.current_pos < self.source.len() {
            self.current_char = self.source[self.current_pos..].chars().next().unwrap_or('\0');

            if self.current_char == '\n' {
                println!("{}: {}", self.line, "");
                self.line += 1;
                self.current_pos += 1;
                continue;
            } else if self.current_char == '#' {
                self.current_pos += 1;
                while self.current_pos < self.source.len() &&
                      self.source[self.current_pos..].chars().next().unwrap_or('\0') != '\n' {
                    self.current_pos += 1;
                }
                continue;
            } else if self.current_char.is_alphabetic() || self.current_char == '_' {
                pp = self.current_pos;
                while self.current_pos < self.source.len() &&
                    (self.source[self.current_pos..].chars().next().unwrap_or('\0').is_alphanumeric() || self.current_char == '_') {
                    self.current_pos += 1;
                }
                let identifier = &self.source[pp..self.current_pos];
                let hash = self.hash_identifier(identifier);
                if let Some(id) = self.symbol_table.get(identifier) {
                    self.tokens.push(id.tk.clone());
                } else {
                    let new_id = Identifier {
                        tk: Token::Id(identifier.to_string()),
                        hash,
                        name: identifier.to_string(),
                        class: DataType::Int,
                        val: 0,
                        is_function: false,
                    };
                    self.symbol_table.insert(identifier.to_string(), new_id);
                    self.tokens.push(Token::Id(identifier.to_string()));
                }
                continue;
            } else if self.current_char.is_digit(10) {
                let mut ival = self.current_char.to_digit(10).unwrap() as i64;
                self.current_pos += 1;
                while self.current_pos < self.source.len() &&
                      self.source[self.current_pos..].chars().next().unwrap_or('\0').is_digit(10) {
                    ival = ival * 10 + self.source[self.current_pos..].chars().next().unwrap_or('0').to_digit(10).unwrap() as i64;
                    self.current_pos += 1;
                }
                self.tokens.push(Token::Num(ival));
                continue;
            } else {
                match self.current_char {
                    '+' => self.tokens.push(Token::Add),
                    '-' => self.tokens.push(Token::Sub),
                    '*' => self.tokens.push(Token::Mul),
                    '/' => self.tokens.push(Token::Div),
                    '=' => self.tokens.push(Token::Assign),
                    _ => {
                        println!("{}: unexpected character '{}'", self.line, self.current_char);
                        std::process::exit(-1);
                    }
                }
                self.current_pos += 1;
            }
        }

        self.tokens.push(Token::Eof);
    }

    fn hash_identifier(&self, identifier: &str) -> usize {
        identifier.chars().fold(0, |hash, c| hash.wrapping_add(c as usize))
    }

    fn expr(&mut self, _lev: usize) {
        if self.tokens.is_empty() {
            println!("{}: unexpected eof in expression", self.line);
            std::process::exit(-1);
        }
        let tk = self.tokens.pop().unwrap();
        if let Token::Num(ival) = tk {
            self.tokens.push(Token::Num(ival)); // IMM
            self.tokens.push(Token::Num(ival));
            self.next();
        } else if let Token::Id(ref id) = tk {
            let d_is_function = self.symbol_table.get(id).map(|i| i.is_function).unwrap_or(false);
            let d_val = self.symbol_table.get(id).map(|i| i.val).unwrap_or(0);
            // Store the last token in a variable
            let last_token = self.tokens.last().cloned();
            if let Some(Token::Id(ref last_id)) = last_token {
                // Now we can safely pop from self.tokens
                if self.tokens.pop().unwrap() == Token::Id(last_id.clone()) {
                    self.next();
                    let mut t = 0;
                    while self.tokens.last() != Some(&Token::Id(")".to_string())) {
                        self.expr(0);
                        t += 1;
                        if self.tokens.last() == Some(&Token::Id(",".to_string())) {
                            self.next();
                        }
                    }
                    self.next();
                    
                    if d_is_function {
                        self.tokens.push(Token::Num(d_val));
                    } else {
                        println!("{}: bad function call", self.line);
                        std::process::exit(-1);
                    }
                    if t > 0 {
                        self.tokens.push(Token::Num(t));
                    }
                }
            }
        } else if tk == Token::Sizeof {
            self.next(); // Move to the next token
            if self.tokens.is_empty() || self.tokens.last() != Some(&Token::Id("(".to_string())) {
                println!("{}: open paren expected in sizeof", self.line);
                std::process::exit(-1);
            }
            self.next(); // Expect an opening parenthesis

        // Handle type checking for sizeof
        if self.tokens.is_empty() || !(self.tokens.last() == Some(&Token::Id("int".to_string())) || self.tokens.last() == Some(&Token::Id("char".to_string()))) {
            println!("{}: expected type after sizeof", self.line);
            std::process::exit(-1);
        }
        self.next(); // Move past the type
        
        // Check for closing parenthesis
        if self.tokens.is_empty() || self.tokens.last() != Some(&Token::Id(")".to_string())) {
            println!("{}: close paren expected in sizeof", self.line);
            std::process::exit(-1);
        }
        self.next(); // Move past the closing parenthesis
        
        // Emit size information (this part may vary based on your implementation)
        self.tokens.push(Token::Num(if self.tokens.last() == Some(&Token::Id("char".to_string())) { std::mem::size_of::<char>() as i64 } else { std::mem::size_of::<i32>() as i64 }));
    } else if tk == Token::Brak {
        self.next(); // Move to the next token
        self.expr(0); // Evaluate the expression inside brackets
        if self.tokens.is_empty() || self.tokens.last() != Some(&Token::Id("]".to_string())) {
            println!("{}: close bracket expected", self.line);
            std::process::exit(-1);
        }
        self.next(); // Move past the closing bracket
        } else {
            println!("{}: unexpected token {:?}", self.line, tk);
            std::process::exit(-1);
        }
    }
    
    fn stmt(&mut self, _lev: usize) {
        let mut a: *mut i32;
        let mut b: *mut i32;
        
        if self.tk == Token::If {
            self.next();
            if self.tk == ')' { self.next(); } else { eprintln!("{}: open paren expected", self.line); std::process::exit(-1); }
            self.expr(Assign);
            
            if tk == ')' { self.next(); } else { eprintln!("{}: close paren expected", line); std::process::exit(-1); }
            *e = BZ; e = e.add(1); b = e;
            stmt();
            
            if tk == Else {
                *b = (e.offset(3) as *mut i32) as i32; *e = JMP; e = e.add(1); b = e;
                next();
                stmt();
            }
            
            *b = (e.add(1) as *mut i32) as i32;
        } else if tk == While {
            next();
            a = e.add(1);
            
            if tk == '(' { next(); } else { eprintln!("{}: open paren expected", line); std::process::exit(-1); }
            expr(Assign);
            
            if tk == ')' { next(); } else { eprintln!("{}: close paren expected", line); std::process::exit(-1); }
            *e = BZ; e = e.add(1); b = e;
            stmt();
            *e = JMP; e = e.add(1); *e = a as i32;
            *b = (e.add(1) as *mut i32) as i32;
        } else if tk == Return {
            next();
            
            if tk != ';' { expr(Assign); }
            *e = LEV; e = e.add(1);
            if tk == ';' { next(); } else { eprintln!("{}: semicolon expected", line); std::process::exit(-1); }
        } else if tk == '{' {
            next();
            while tk != '}' { stmt(); }
            next();
        } else if tk == ';' {
            next();
        } else {
            expr(Assign);
            
            if tk == ';' { next(); } else { eprintln!("{}: semicolon expected", line); std::process::exit(-1); }
        }
    }
}


    fn main() {
 
}

