use std::collections::HashMap;

#[derive(Debug, Clone)]
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

#[derive(Debug)]
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

#[derive(Debug)]
enum DataType {
    Char,
    Int,
    Ptr,
}

#[derive(Debug)]
struct Identifier {
    tk: Token,
    hash: usize,
    name: String,
    class: DataType,
    val: i64,
}

struct Compiler {
    source: String,
    current_pos: usize,
    line: usize,
    tokens: Vec<Token>,
    symbol_table: HashMap<String, Identifier>,
    current_char: char,
}

impl Compiler {
    fn new(source: String) -> Self {
        Compiler {
            source,
            current_pos: 0,
            line: 1,
            tokens: Vec::new(),
            symbol_table: HashMap::new(),
            current_char: '\0', // Initialize with a null character
        }
    }

    fn next(&mut self) {
        let mut pp: usize; // Pointer to hold the start of an identifier or string

        while self.current_pos < self.source.len() {
            self.current_char = self.current_char(); // Get the current character

            if self.current_char == '\n' {
                // Check for a newline character
                println!(
                    "{}: {}",
                    self.line,
                    &self.source[self.current_pos..self.current_pos]
                );
                self.line += 1; // Increment line number
                self.current_pos += 1; // Move to the next character
                continue;
            } else if self.current_char == '#' {
                // Skip comments
                self.current_pos += 1;
                while self.current_pos < self.source.len() && self.current_char() != '\n' {
                    self.current_pos += 1;
                }
                continue;
            } else if self.current_char.is_alphabetic() || self.current_char == '_' {
                // Handle identifiers
                pp = self.current_pos; // Store the starting position of the identifier
                while self.current_pos < self.source.len()
                    && (self.current_char().is_alphanumeric() || self.current_char() == '_')
                {
                    self.current_pos += 1; // Read the identifier
                }
                let identifier = &self.source[pp..self.current_pos];
                let hash = self.hash_identifier(identifier);
                // Check if the identifier exists in the symbol table
                if let Some(id) = self.symbol_table.get(identifier) {
                    self.tokens.push(id.tk.clone());
                } else {
                    // If not found, add the new identifier to the symbol table
                    let new_id = Identifier {
                        tk: Token::Id(identifier.to_string()),
                        hash,
                        name: identifier.to_string(),
                        class: DataType::Int, // Default class, can be changed
                        val: 0,               // Default value
                    };
                    self.symbol_table.insert(identifier.to_string(), new_id);
                    self.tokens.push(Token::Id(identifier.to_string()));
                }
                continue;
            } else if self.current_char.is_digit(10) {
                // Handle numbers
                let mut ival = self.current_char.to_digit(10).unwrap() as i64;
                self.current_pos += 1;
                while self.current_pos < self.source.len() && self.current_char().is_digit(10) {
                    ival = ival * 10 + self.current_char().to_digit(10).unwrap() as i64;
                    self.current_pos += 1;
                }
                self.tokens.push(Token::Num(ival));
                continue;
            } else {
                // Handle operators and other characters
                match self.current_char {
                    '=' => {
                        self.current_pos += 1;
                        if self.current_char() == '=' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Eq);
                        } else {
                            self.tokens.push(Token::Assign);
                        }
                    }
                    '+' => {
                        self.current_pos += 1;
                        if self.current_char() == '+' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Inc);
                        } else {
                            self.tokens.push(Token::Add);
                        }
                    }
                    '-' => {
                        self.current_pos += 1;
                        if self.current_char() == '-' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Dec);
                        } else {
                            self.tokens.push(Token::Sub);
                        }
                    }
                    '!' => {
                        self.current_pos += 1;
                        if self.current_char() == '=' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Ne);
                        }
                    }
                    '<' => {
                        self.current_pos += 1;
                        if self.current_char() == '=' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Le);
                        } else {
                            self.tokens.push(Token::Lt);
                        }
                    }
                    '>' => {
                        self.current_pos += 1;
                        if self.current_char() == '=' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Ge);
                        } else {
                            self.tokens.push(Token::Gt);
                        }
                    }
                    '|' => {
                        self.current_pos += 1;
                        if self.current_char() == '|' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Lor);
                        }
                    }
                    '&' => {
                        self.current_pos += 1;
                        if self.current_char() == '&' {
                            self.current_pos += 1;
                            self.tokens.push(Token::Lan);
                        }
                    }
                    '^' => {
                        self.tokens.push(Token::Xor);
                    }
                    '%' => {
                        self.tokens.push(Token::Mod);
                    }
                    '*' => {
                        self.tokens.push(Token::Mul);
                    }
                    '[' => {
                        self.tokens.push(Token::Brak);
                    }
                    '?' => {
                        self.tokens.push(Token::Cond);
                    }
                    _ => {
                        // Ignore unrecognized characters
                    }
                }
                self.current_pos += 1; // Move to the next character
            }
        }
        self.tokens.push(Token::Eof); // End of file
    }

    fn current_char(&self) -> char {
        self.source[self.current_pos..]
            .chars()
            .next()
            .unwrap_or('\0')
    }

    fn hash_identifier(&self, identifier: &str) -> usize {
        // Simple hash function for identifiers
        identifier
            .chars()
            .fold(0, |hash, c| hash.wrapping_mul(31).wrapping_add(c as usize))
    }
    
}

fn main() {
    //SARA THIS IS JUST A RANDOM TEST SO I CAN RUN THE CODE WE NEED TO CHANGE IT LATER 

    let source_code = "
        int main() {
            // This is a comment
            int x = 10;
            if (x > 5) {
                x++;
            }
            return x;
        }
    ";

    let mut compiler = Compiler::new(source_code.to_string());
    while compiler.current_pos < compiler.source.len() {
        compiler.next();
        println!("Current token: {:?}", compiler.tokens.pop());
    }
}
