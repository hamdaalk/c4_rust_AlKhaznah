use std::collections::HashMap;
use std::fs;
use std::io::{self, Read};
use std::ptr;

//Classes and Tokens 
#[derive(Debug, PartialEq, Clone, Copy)]
enum Token {
    Num(i64),
    Fun,
    Sys,
    Glo,
    Loc,
    Id,
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
    Operator(char),
    EOF,
}

//opcodes
#[derive(Debug, Clone, Copy)]
enum Opcode{
    LEA, IMM, JMP, JSR, BZ, BNZ, ENT, ADJ, LEV, LI, LC, SI, SC, PSH,
    OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD,
    OPEN, READ, CLOS, PRTF, MALC, FREE, MSET, MCMP, EXIT,
}

//Types
#[derive(Debug, Clone, Copy, PartialEq)]
enum Type{
    Char,
    Int,
    Ptr(Box<Type>),
}

// Identifier structure
#[derive(Debug)]
struct Identifier {
    token: Token,
    hash: u64,
    name: String,
    class: i32,  // Would use enum in more complete implementation
    type_: Type,
    value: i64,
    hclass: i32,
    htype: Type,
    hval: i64,
}

struct Compiler {
    source: Vec<char>,
    position: usize,
    line: usize,
    
    // Virtual machine state
    data: Vec<u8>,
    stack: Vec<i64>,
    pc: usize,
    
    // Symbol table
    symbols: HashMap<String, Identifier>,
    
    // Compiler state
    token: Token,
    token_val: i64,
    current_type: Type,
    local_offset: i64,
    
    // Flags
    print_source: bool,
    print_assembly: bool,
    debug: bool,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            Source: Vec::new(),
            position: 0,
            line: 1,
            data: Vec::new(),
            stack: Vec:: new(),
            pc: 0,
            symbols: HashMap::new(),
            token: Token::EOF,
            token_val: 0,
            current_type: Type::Int,
            local_offset: 0,
            print_source: false,
            print_assembly: false,
            debug: false,
        }
    }
    pub fn load_file(&mut self, filename: &str) -> io::Result<()> {
        let contents = fs::read_to_string(filename)?;
        self.source = contents.chars().collect();
        Ok(())
    }

    fn next_char(&mut self) -> Option<char> {
        if self.position < self.source.len() {
            let ch = self.source[self.position];
            self.position += 1;
            if ch == '\n' {
                self.line += 1;
            }
            Some(ch)
        } else {
            None
        }
    } 

    fn peek_char(&self) -> Option<char> {
        if self.position < self.source.len() {
            Some(self.source[self.position])
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn parse_identifier(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(ch) = self.peek_char() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(self.next_char().unwrap());
            } else {
                break;
            }
        }
        
        match ident.as_str() {
            "char" => Token::Char,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "if" => Token::If,
            "int" => Token::Int,
            "return" => Token::Return,
            "sizeof" => Token::Sizeof,
            "while" => Token::While,
            _ => {
                // Look up in symbol table
                self.token_val = 0; // Would store hash or other identifier info
                Token::Id
            }
        }
    }

    fn parse_number(&mut self) -> Token {
        let mut num = 0i64;
        while let Some(ch) = self.peek_char() {
            if let Some(digit) = ch.to_digit(10) {
                num = num * 10 + digit as i64;
                self.next_char();
            } else {
                break;
            }
        }
        self.token_val = num;
        Token::Num(num)
    }

    fn parse_string(&mut self) -> Token {
        let quote = self.next_char().unwrap(); // Consume opening quote
        let mut s = String::new();
        
        while let Some(ch) = self.next_char() {
            if ch == quote {
                break;
            }
            s.push(ch);
        }
        
        // In real implementation, would handle string literals properly
        self.token_val = 0; // Would store string address
        Token::Num(0) // Simplified for this example
    }

    fn parse_comment_or_div(&mut self) -> Token {
        let first_slash = self.next_char().unwrap();
        match self.peek_char() {
            Some('/') => {
                // Line comment
                while let Some(ch) = self.next_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                self.next_token() // Skip to next token after comment
            }
            Some('*') => {
                // Block comment
                self.next_char(); // Consume '*'
                let mut last = '\0';
                while let Some(ch) = self.next_char() {
                    if last == '*' && ch == '/' {
                        break;
                    }
                    last = ch;
                }
                self.next_token() // Skip to next token after comment
            }
            _ => Token::Operator('/'), // Division operator
        }
    }
    
    fn parse_operator(&mut self) -> Token {
        let first = self.next_char().unwrap();
        let second = self.peek_char();
        
        match (first, second) {
            ('=', Some('=')) => { self.next_char(); Token::Eq },
            ('!', Some('=')) => { self.next_char(); Token::Ne },
            ('<', Some('=')) => { self.next_char(); Token::Le },
            ('>', Some('=')) => { self.next_char(); Token::Ge },
            ('<', Some('<')) => { self.next_char(); Token::Shl },
            ('>', Some('>')) => { self.next_char(); Token::Shr },
            ('|', Some('|')) => { self.next_char(); Token::Lor },
            ('&', Some('&')) => { self.next_char(); Token::Lan },
            ('+', Some('+')) => { self.next_char(); Token::Inc },
            ('-', Some('-')) => { self.next_char(); Token::Dec },
            _ => match first {
                '=' => Token::Assign,
                '!' => Token::Operator('!'),
                '<' => Token::Lt,
                '>' => Token::Gt,
                '|' => Token::Or,
                '&' => Token::And,
                '+' => Token::Add,
                '-' => Token::Sub,
                '*' => Token::Mul,
                '%' => Token::Mod,
                _ => Token::Operator(first),
            }
        }
    }

    fn next(&mut self) {
        loop {
            while let Some(&ch) = self.peek_char() {
                if !ch.is_whitespace() {
                    break;
                }
                if ch == '\n' {
                    self.line += 1;
                    if self.print_source {
                        //printing the source line and the assembly will go here   
                    }
                }

                self.next_char();
            }

            let tk_char =match self.next_char() {
                Some(c) => c,
                None => {
                    self.token = token::EOF;
                    return;
                }
            };

            match tk_char {
                //preprocessor directives handler
                '#' => {
                    while let Some(&ch) = self.peek_char() {
                        if ch == '\n' || ch == '\0' {
                            break;
                        }
                        self.next_char();
                    }
                    continue;
                }

                c if c.is_alphabetic() || c == '_' => {
                    let start = self.position - 1;
                    while let Some(&ch) = self.peek_char() {
                        if ch.is_alphabetic() || ch == '_' {
                            self.next_char();
                        } else {
                            break;
                        }
                    }
                    let ident: String = self.source[start..self.position].iter().collect();

                    //checking the keywords
                    self.token = match ident.as_str() {
                        "char" => Token::Char,
                        "else" => Token::Else,
                        "enum" => Token::Enum,
                        "if" => Token::If,
                        "int" => Token::Int,
                        "return" => Token::Return,
                        "sizeof" => Token::Sizeof,
                        "while" => Token::While,
                        _ => {
                            self.token_val = self.symbols.get(&ident).map_or(0, |id| id.value);
                            Token::Id
                        }
                    };
                    return;
                }

                c if c.is_ascii_digit() => {
                    let mut value = (c as u8 - b'0') as i64;
                    //decimal numbers
                    if value != 0 {
                        while let some(&ch) = self.peek_char() {
                            if ch.is_ascii_digit() {
                                value = value *10 + (ch as u8 - b'0') as i64;
                                self.next_char();
                            } else {
                                break;
                            }
                        }
                    }
                    else if let Some(& 'x') = self.peek_char() {
                        self.next_char();
                        value = 0;
                        while let Some(&ch) = self.peek_char() {
                            match ch.to_ascii_lowercase() {
                                '0' ..= '9' => {
                                    value = value *16 + (ch as u8 -b'0') as i64;
                                    self.next_char();
                                }
                                'a'..='f' => {
                                    value = value *16 +(ch as u8 -b'a' +10) as i64;
                                    self.next_char();
                                }
                                _ => break,
                            }
                        }
                    }
                    else {
                        while let Some(&ch) = self.peek_char() {
                            if ('0'..='7').contains(&ch) {
                                value = vlaue *8 + (ch as u8 - b'0') as i64;
                                self.next_char();
                            } else {
                                break;
                            }
                        }
                    }
                    self.token_val =value;
                    self.token = Token::Num(value);
                    return;
                }
                // to handle strings and characters 
                '"' | '\'' => {
                    let is_string = tk_char == '"';
                    let start = self.data.len();
                    while let Some(&ch) = self.peek_char(){
                        if ch == tk_char{
                            break;
                        }
                        let mut val = ch;
                        self.next_char();
                        
                        // to handle escape sequence 
                        if val == '\\' {
                            val = match self.next_char().unwrap_or('\\') {
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                '0' => '\0',
                                c => c,
                            };
                        }
                        if is_string {
                            self.data.push(val as u8);
                        } else {
                            self.token_val = val as i64;
                        }
                    }
                    self.next_char(); //To skip the closing quote
                    if is_string{
                        self.token_val = start as i64;
                        self.token = Token::Num(self.token_val);
                    } else {
                        self.token = Token::Num(self.token_val);
                    }
                    return;
                }

                //to handle comments and division 

                '/' => {
                    match self.peek_char() {
                        Some('/') => {
                            self.next_char();
                            while let Some(&ch) = self.peek_char() {
                                if ch == '\n' {
                                    break;
                                }
                                self.next_char();
                            }
                            continue;
                        }
                        Some('*') => {
                            self.next_char();
                            let mut last = '\0';
                            while let Some(&ch) = self.peek_char() {
                                if last == '*' && ch =='/' {
                                    self.next_char();
                                    break;
                                }
                                last = ch;
                                self.next_char();
                            }
                            continue;
                        }
                        _ => {
                            self.token = Token::Div;
                            return;
                        }
                    }
                }
                //to handle multi-char operators
                '=' => {
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        self.token = Token::Eq;
                    } else {
                        self.token = Token::Assign;
                    }
                    return;
                }

                '+' => {
                    if let Some('+') = self.peek_char() {
                        self.next_char();
                        self.token = Token::Inc;
                    } else {
                        self.token = Token::Add;
                    }
                    return;
                }

                '-' => {
                    if let Some('-') = self.peek_char() {
                        self.next_char();
                        self.token = Token::Dec;
                    } else {
                        self.token = Token::Sub;
                    }
                    return;
                }

                '<' => {
                    match self.peek_char() {
                        Some('=') => {
                            self.next_char();
                            self.token = Token::Le;
                        }
                        Some('<') => {
                            self.next_char();
                            self.token = Token::Shl;
                        }
                        _ => {
                            self.token = Token::Lt;
                        }
                    }
                    return;
                }

                '>' => {
                    match self.peek_char() {
                        Some('=') => {
                            self.next_char();
                            self.token = Token::Ge;
                        }
                        Some('>') => {
                            self.next_char();
                            self.token = Token::Shr;
                        }
                        _ => {
                            self.token = Token::Gt;
                        }
                    }
                    return;
                }
                '|' => {
                    if let Some('|') = self.peek_char() {
                        self.next_char();
                        self.token = Token::Lor;
                    } else {
                        self.token = Token::Or;
                    }
                    return;
                }

                '&' => {
                    if let Some('&') = self.peek_char() {
                        self.next_char();
                        self.token = Token::Lan;
                    } else {
                        self.token = Token::And;
                    }
                    return;
                }

                // Single-character tokens
                '^' => {
                    self.token = Token::Xor;
                    return;
                }
                '%' => {
                    self.token = Token::Mod;
                    return;
                }
                '*' => {
                    self.token = Token::Mul;
                    return;
                }
                '[' => {
                    self.token = Token::Brak;
                    return;
                }
                '?' => {
                    self.token = Token::Cond;
                    return;
                }

                c @ ('~' | ';' | '{' | '}' | '(' | ')' | ']' | ',' | ':') => {
                    self.token = Token::Operator(c);
                    return;
                }

                _ => continue,

            }
        }
    }

    fn expr(&mut self, lev: i32) -> Result<(), String> {
        if self.token == Token::EOF {
            return Err(format!("{}: unexpected EOF in expression", self.line));
        }

        match self.token{
            Token::Num(value) => {
                self.emit(Opcode::IMM);
                self.emit_operand(value);
                self.next_token()?;
                self.current_type = Type::Int;
            }

            Token::Operator('"') => {
                self.emit(Opcode::IMM);
                self.emit_operand(value);
                self.next_token()?;
                // to handle string concatenation
                while self.token == Token::Operator('"') {
                    self.next_token()?;
                }
                self.data_ptr = (self.data_ptr + std::mem::size_of::<i32>() -1) & !(std::mem::size_of::<i32>() -1);
                self.current_type = Type::Ptr(Box::new(Type::char));    
            }

            Token::Sizeof => {
                self.next_token()?;
                if self.token != Token::Operator('(') {
                    return Err(format!("{}: open paranthesis expected in size of", self.line));
                }
                self.next_token()?;
                self.current_type =Type::Int;
                match self.token {
                    Token::Int => {
                        self.next_token()?;
                    }
                    Token::Char => {
                        self.next_token()?;
                        self.current_type = Type::Char;
                    }
                    _ => {}
                }

                //to handle pointer typs 
                while self.token == Token::Operator('*') {
                    self.next_token()?;
                    self.current_type = Type::Ptr(Box::new(self.current_type));
                }

                if self.token != Token::Operator(')') {
                    return Err(format!("{}: closed paranthesis expected" self.line));
                }

                self.next_token()?;
                
                let size = match self.current_type {
                    Type::Char => 1,
                    _ => std::mem::size_of::<i32>() as i64,

                };

                self.emit(Opcode::IMM);
                self.emit_operand(size);
                self.current_type = Type::Int;
            }
            Token::Id => {
                let id = self.current_id.clone();
                self.next_token()?;
                if self.next_token == Token::Operator('(') {
                    self.next_token()?;
                    let mut arg_count =0;

                    while self.token != Token::Operator('(') {
                        self.expr(Precedence::Assign)?;
                        self.emit(Opcode::PSH);
                        arg_count += 1;

                        if self.token == Token::Operator(',') {
                            self.next_token()?;
                        }
                    }
                    self.next_token()?;

                    match id.class {
                        Class::Sys => {
                            self.emit_syscall(id.value);
                        }
                        Class:: Fun => {
                            self.emit(Opcode::JSR);
                            self.emit_operand(id.value);
                        }
                        _ => {
                            return Err(format!("{}: bad function call", self.line));
                        }
                    }

                    if arg_count > 0 {
                        self.emit(Opcode::ADJ);
                        self.emit_operand(arg_count);
                    }
                    self.current_type = id.type_; 
                }
                else if let Class::Num = id.class {
                    self.emit(Opcode::IMM);
                    self.emit_operand(id.value);
                    self.current_type = Type::Int;
                }
                else {
                    match id.class {
                        Class::Loc => {
                            self.emit(Opcode::LEA);
                            self.emit_operand(self.local_offset - id.value);
                        }
                        Class::Glo => {
                            self.emit(Opcode::IMM);
                            self.emit_operand(id.value);
                        }
                        _ => {
                            return Err(format!("{}: undefined variable", self.line));
                        }
                    }

                    self.current_type = id.type_;
                    match self.current_type {
                        Type::Char => self.emit(Opcode::LC),
                        _ => self.emit(Opcode::LI),
                    }
                }
            }

            Token::Operator('(') => {
                self.next_token()?;
                if matches!(self.token, Token::Int | Token::Char) {
                    let mut cast_type = if self.token == Token::Int {
                        Type::Int
                    } else {
                        Type::Char
                    };
                    self.next_token()?;

                    while self.token == Token::Operator('*') {
                        self.next_token()?;
                        cast_type = Type::Ptr(Box::new(cast_type));
                    }
                    if self.token != Token::Operator(')') {
                        return Err(format!("{}: Bad Cast", self.line));
                    }
                    self.next_token()?;
                    self.expr(Precedence::Inc)?;
                    self.current_type = cast_type;
                }
                else {
                    self.expr(Precedence::Assign)?;
                    if self.token != Token::Operator(')') {
                        return Err(format!("{}: close parenthsis expected", self.line));
                    }
                    self.next_token()?;
                }
            }
            Token::Operator('*') => {
                //Dereferencing
                self.next_token()?;
                self.expr(Precedence::Inc)?;
                if let Type::Ptr(inner) = self.current_type {
                    self.current_type = *inner;
                } else {
                    return Err(format!("{}: bad dereference", self.line));
                }
                match self.current_type {
                    Type::Char => self.emit(Opcode::LC),
                    _ => self.emit(Opcode::LI),
                }
            }
            Token::Operator('&') => {
                self.next_token()?;
                self.expr(Precedence::Inc)?;
                if !matches!(self.last_op, Opcode::LC | Opcode::LI) {
                    return Err(format!("{}: bad address", self.line));
                }
                //remove the last load operation
                self.code.pop();
                self.code.pop();
                self.current_type = Type::Ptr(Box::new(self.current_type));
            }
            Token::Operator('!') => {
                self.next_token()?;
                self.expr(Precedence::Inc)?;
                self.emit(Opcode::PSH);
                self.emit(Opcode::IMM);
                self.emit_operand(0);
                self.emit(Opcode::EQ);
                self.current_type = Type::Int;
            }
            Token::Operator('~') => {
                self.next_token()?;
                self.expr(Precedence::Inc)?;
                self.emit(Opcode::PSH);
                self.emit(Opcode::IMM);
                self.emit_operand(-1);
                self.emit(Opcode::XOR);
                self.current_type = Type::Int;
            }
            Token::Operator('+') => {
                self.next_token()?;
                self.expr(Precedence::Inc)?;
                self.current_type = Type::Int;
            }
            Token::Operator('-') => {
                self.next_token()?;
                if let Token::Num(value) = self.token {
                    self.emit(Opcode::IMM);
                    self.emit_operand(-value);
                    self.next_token()?;
                } else {
                    self.emit(Opcode::IMM);
                    self.emit_operand(-1);
                    self.emit(Opcode::PSH);
                    self.expr(Precedence::Inc)?;
                    self.emit(Opcode::MUL);
                }
                self.current_type = Type::Int;
            }
            Token::Inc | Token::Dec => {
                let op = self.token;
                self.next_token()?;
                self.expr(Precedence::Inc)?;
                if !matches!(self.last_op, Opcode::LC | Opcode::LI) {
                    return Err(format!("{}: bad l-value in pre-increment", self.line));
                }
                let size = match self.current_type {
                    Type::Ptr(_) => std::mem::size_of::<i32>() as i64,
                    Type::Char => 1,
                    _ => 1,
                };

                self.emit(Opcode::PSH);
                self.emit(Opcode::IMM);
                self.emit_operand(size);
                match op {
                    Token::Inc => self.emit(Opcode::ADD),
                    Token::Dec => self.emit(Opcode::SUB),
                    _ => unreachable!(),
                }
                match self.current_type {
                    Type::Char => self.emit(Opcode::SC),
                    _ => self.emit(Opcode::SI),
                }
            }

            _ => {
                return Err(format!("{}: bad expression", self.line));
            }
        }

        while self.token.precedence() >= lev {
            let left_type = self.current_type;
            match self.token {
                Token::Assign => {
                    self.next_token()?;
                    if !matches!(self.last_op, Opcode::LC | Opcode::LI) {
                        self.emit(Opcode::PSH);
                    }
                    self.expr(Precedence::Assign)?;
                    self.current_type = left_type;
                    match left_type {
                        Type::Char => self.emit(Opcode::SC),
                        _ => self.emit(Opcode::SI),
                    }
                }

                _ => {
                    return Err(format!("{}: compiler error", self.line));
                }
            }
        }

        Ok(())
    }
    fn emit(&mut self, op: Opcode){
        self.code.push(op as i64);
        self.last_op = op;
    }
    fn emit_operand(&mut self, value: i64){
        self.code.push(value);
    }
    fn emit_syscall(&mut self, syscall_num: i64) {
        self.emit(Opcode::IMM);
        self.emit_operand(syscall_num);
        self.emit(Opcode::EXIT);
    }   

}

