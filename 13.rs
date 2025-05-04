use std::collections::HashMap; 
use std::fs;
use std::io;

//use std::collections::HashMap;
use std::fs::File;
use std::io::{Read};
use std::convert::TryInto;


// same 256 KiB as in the C version
const POOLSZ: usize = 256 * 1024;

//-----------------------------------------------------------------------------
// 1) Operator precedences
//-----------------------------------------------------------------------------
#[derive(Debug, Clone, Copy)]
enum Precedence {
    Assign = 1,
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
    Inc,
}

//-----------------------------------------------------------------------------
// 2) Token definitions
//-----------------------------------------------------------------------------
#[derive(Debug, Clone, Copy, PartialEq)]
enum Token {
    Num(i64),
    Float(f64), //float literals
    Id,
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    Assign, Cond, Lor, Lan, Or, Xor, And,
    Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr,
    Add, Sub, Mul, Div, Mod, Inc, Dec,
    Brak,           // '['
    Operator(char),// punctuation & single-char ops: ; , (){} etc.
    EOF,
}

impl Token {
    fn precedence(&self) -> i32 {
        use Token::*;
        match self {
            Assign => Precedence::Assign   as _,
            Cond   => Precedence::Cond     as _,
            Lor    => Precedence::Lor      as _,
            Lan    => Precedence::Lan      as _,
            Or     => Precedence::Or       as _,
            Xor    => Precedence::Xor      as _,
            And    => Precedence::And      as _,
            Eq     => Precedence::Eq       as _,
            Ne     => Precedence::Ne       as _,
            Lt     => Precedence::Lt       as _,
            Gt     => Precedence::Gt       as _,
            Le     => Precedence::Le       as _,
            Ge     => Precedence::Ge       as _,
            Shl    => Precedence::Shl      as _,
            Shr    => Precedence::Shr      as _,
            Add    => Precedence::Add      as _,
            Sub    => Precedence::Sub      as _,
            Mul    => Precedence::Mul      as _,
            Div    => Precedence::Div      as _,
            Mod    => Precedence::Div      as _,
            Inc|Dec=> Precedence::Inc      as _,
            _      => 0,
        }
    }
}

//-----------------------------------------------------------------------------
// 3) VM opcodes
//-----------------------------------------------------------------------------
#[repr(i64)]

#[derive(Debug, Clone, Copy)]
enum Opcode {
    LEA, IMM, JMP, JSR, BZ, BNZ, ENT, ADJ, LEV,
    LI, LC, SI, SC, PSH,
    OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR,
    ADD, SUB, MUL, DIV, MOD,
    FADD, FSUB, FMUL, FDIV, //Floating point ops
    OPEN, READ, CLOS, PRTF, MALC, FREE, MSET, MCMP, EXIT,
    NOP,
}

//-----------------------------------------------------------------------------
// 4) Types, symbol classes, and identifier entries
//-----------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
enum Type {
    Char,
    Int,
    Float,             // <— new
    Ptr(Box<Type>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Class {
    Sys, Fun, Glo, Loc, Num,
}

#[derive(Debug)]
struct Identifier {
    token: Token,
    hash:  u64,
    name:  String,
    class: Class,
    type_: Type,
    value: i64,
    hclass: Class,
    htype:  Type,
    hval:   i64,
}

//-----------------------------------------------------------------------------
// 5) The Compiler struct & constructor
//-----------------------------------------------------------------------------
struct Compiler {
    // source buffer + cursor
    source:    Vec<char>,
    position:  usize,
    line:      usize,
    // —— four fixed‐size arenas (malloc+memset in C) —— 
    sym_pool:   Vec<u8>,    // was `sym = malloc(poolsz)`
    text_pool:  Vec<i64>,   // was `e     = malloc(poolsz)`
    data_pool:  Vec<u8>,    // was `data  = malloc(poolsz)`
    stack_pool: Vec<i64>,   // was `sp    = malloc(poolsz)`
    // VM memory
    data:      Vec<u8>,
    stack:     Vec<i64>,
    code:      Vec<i64>,
    last_op:   Opcode,
    data_ptr:  usize,
    pc:        usize,
        // for Option 2 library calls:
    fd_table:   HashMap<i64, File>,
    heap_alloc: Vec<u8>,
    
    // symbol table
    symbols:   HashMap<String, Identifier>,
    // parser state
    token:       Token,
    token_val:   i64,
    ident:       String,
    current_type: Type,
    local_offset: i64,
    // flags
    print_source:   bool,
    print_assembly: bool,
    debug:          bool,
}

impl Compiler {

    pub fn new() -> Self {
        // —— 1) allocate & zero the four arenas —— 
        let sym_pool   = vec![0u8; POOLSZ];
        let text_pool  = vec![0i64; POOLSZ / std::mem::size_of::<i64>()];
        let data_pool  = vec![0u8; POOLSZ];
        let stack_pool = vec![0i64; POOLSZ / std::mem::size_of::<i64>()];

        // —— 2) now construct the Compiler with those arenas plus your existing fields —— 
        Compiler {
            // source buffer + cursor
            source:        Vec::new(),
            position:      0,
            line:          1,

            // our new pools
            sym_pool,
            data_pool,
            text_pool,
            stack_pool,

            // existing VM buffers and state
            data:         vec![0u8; POOLSZ], // allocate real data memory
            stack:         Vec::new(),
            code:          Vec::new(),
            last_op:       Opcode::NOP,
            data_ptr:      0,
            pc:            0,
            // for Option 2 library calls
            fd_table:      HashMap::new(),
            heap_alloc:    Vec::new(),
            // symbol table & parser state
            symbols:       HashMap::new(),
            token:         Token::EOF,
            token_val:     0,
            ident:         String::new(),
            current_type:  Type::Int,
            local_offset:  0,

            // flags
            print_source:   false,
            print_assembly: false,
            debug:          false,
        }
    }

    fn emit(&mut self, op: Opcode) {
        self.code.push(op as i64);
        self.last_op = op;
    }

    /// Push a 64‐bit immediate operand into the code buffer
    fn emit_operand(&mut self, v: i64) {
        self.code.push(v);
    }
    fn emit_syscall(&mut self, syscall_num: i64) {
        // 1) push the syscall number as an immediate
        self.emit(Opcode::IMM);
        self.emit_operand(syscall_num);
        // 2) emit EXIT, which your VM loop will interpret as “invoke syscall”
        self.emit(Opcode::EXIT);
    }

    pub fn load_file(&mut self, filename: &str) -> io::Result<()> {
        let s = fs::read_to_string(filename)?;
        self.source = s.chars().collect();
        Ok(())
    }

    // get next char, update line count
    fn next_char(&mut self) -> Option<char> {
        if self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;
            if c == '\n' { self.line += 1; }
            Some(c)
        } else {
            None
        }
    }
    fn peek_char(&self) -> Option<char> {
        self.source.get(self.position).copied()
    }
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
            } else { break; }
        }
    }

    //----------------------------------------------------------------------------- 
    // 6) Tokenizer: next()
    //-----------------------------------------------------------------------------
/// Advances the tokenizer to the next token in the source code.
/// - Skips whitespace and comments.
/// - Identifies and categorizes tokens (e.g., numbers, identifiers, operators).
/// - Updates the `self.token` field with the current token and its value.
    fn next(&mut self) {
        loop {
    // 1) Skip whitespace & handle newlines
            while let Some(ch) = self.peek_char() {
                if ch == '\n' {
                    // consume exactly one newline
                    self.next_char();
                    self.line += 1;
                    if self.print_source {
                        // (optional) print the source line here
                        let end = self.position - 1; // position of the '\n'
                        let mut start = end;
                        while start > 0 && self.source[start - 1] != '\n' {
                            start -= 1;
                        }
                        let line_str: String = self.source[start..end].iter().collect();
                        println!("{:4} {}", self.line, line_str);
                    }
                    continue;
                }
                if ch.is_whitespace() {
                    self.next_char();
                    continue;
                }
                break;
            }
    
            // 2) End of input?
            let ch = match self.next_char() {
                Some(c) => c,
                None    => { self.token = Token::EOF; return; }
            };
    
            // 3) Preprocessor‐style directive: skip to end of line
            if ch == '#' {
                while let Some(c2) = self.next_char() {
                    if c2 == '\n' {
                        self.line += 1;
                        break;
                    }
                }
                continue;
            }
    
            // 4) Identifier or keyword
            if ch.is_alphabetic() || ch == '_' {
                let mut id = String::new();
                id.push(ch);
                while let Some(c2) = self.peek_char() {
                    if c2.is_alphanumeric() || c2 == '_' {
                        id.push(self.next_char().unwrap());
                    } else {
                        break;
                    }
                }
                self.ident = id.clone();
                self.token = match id.as_str() {
                    "char"   => Token::Char,
                    "else"   => Token::Else,
                    "enum"   => Token::Enum,
                    "if"     => Token::If,
                    "int"    => Token::Int,
                    "return" => Token::Return,
                    "sizeof" => Token::Sizeof,
                    "while"  => Token::While,
                    other    => {
                        let hash = other.bytes()
                            .fold(0u64, |h, b| h.wrapping_mul(31).wrapping_add(b as u64));
                        let entry = self.symbols.entry(other.to_string())
                            .or_insert_with(|| Identifier {
                                token:  Token::Id,
                                hash,
                                name:   other.to_string(),
                                class:  Class::Glo,
                                type_:  Type::Int,
                                value:  0,
                                hclass: Class::Glo,
                                htype:  Type::Int,
                                hval:   0,
                            });
                        self.token_val = entry.value;
                        Token::Id
                    }
                };
                return;
            }
            // 5) Number literal (decimal, hex, or octal and now float)
            if ch.is_ascii_digit() {
                let mut v = (ch as u8 - b'0') as i64;
                let mut is_float = false;
                let mut float_val = 0.0;
                
                // 0x… hex?
                if v == 0 && self.peek_char()==Some('x') {
                    self.next_char();
                    v = 0;
                    while let Some(c2) = self.peek_char() {
                        if let Some(d) = c2.to_digit(16) {
                            v = v*16 + d as i64;
                            self.next_char();
                        } else {
                            break;
                        }
                    }
                }
                // octal (leading 0)  
                else if v == 0 {
                    while let Some(c2) = self.peek_char() {
                        if ('0'..='7').contains(&c2) {
                            v = v*8 + (c2 as u8 - b'0') as i64;
                            self.next_char();
                        } else {
                            break;
                        }
                    }
                }
                // Parse integer part
                while let Some(c2) = self.peek_char() {
                    if c2.is_ascii_digit() {
                        v = v * 10 + (self.next_char().unwrap() as u8 - b'0') as i64;
                    } else if c2 == '.' {
                        is_float = true;
                        self.next_char(); // consume '.'
                        break;
                    } else {
                        break;
                    }
                }
            
                // Parse fractional part if it's a float
                if is_float {
                    let mut divisor = 10.0;
                    float_val = v as f64;
                    while let Some(c2) = self.peek_char() {
                        if c2.is_ascii_digit() {
                            float_val += (self.next_char().unwrap() as u8 - b'0') as f64 / divisor;
                            divisor *= 10.0;
                        } else {
                            break;
                        }
                    }
                    self.token_val = 0; // Not used for floats
                    self.token = Token::Float(float_val);
                }

                // decimal
                else {
                    while let Some(c2) = self.peek_char() {
                        if c2.is_ascii_digit() {
                            v = v*10 + (self.next_char().unwrap() as u8 - b'0') as i64;
                        } else {
                            break;
                        }
                    }
                }

                self.token_val = v;
                self.token     = Token::Num(v);
                return;
            }

            // 6) String or character literal (with escapes)
            if ch == '"' || ch == '\'' {
                let quote = ch;
                let start = self.data_pool.len();
                while let Some(c2) = self.peek_char() {
                    self.next_char();
                    if c2 == quote {
                        break;
                    }
                    let real = if c2 == '\\' {
                        match self.next_char().unwrap_or('\\') {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '0' => '\0',
                            other => other,
                        }
                    } else {
                        c2
                    };
                    self.data_pool.push(real as u8);
                }
                // return the address of the string in data_pool
                self.token_val = start as i64;
                self.token     = Token::Num(self.token_val);
                self.data_pool.push(0);

                return;
            }

            // 7) Comment vs slash
            if ch == '/' {
                if let Some('/') = self.peek_char() {
                    // lsine comment
                    self.next_char();
                    while let Some(c2) = self.next_char() {
                        if c2 == '\n' {
                            break;
                        }
                    }
                    continue;
                }
                if let Some('*') = self.peek_char() {
                    // block comment
                    self.next_char();
                    let mut last = '\0';
                    while let Some(c2) = self.next_char() {
                        if last == '*' && c2 == '/' {
                            break;
                        }
                        last = c2;
                    }
                    continue;
                }
                // else it really is a division operator
                self.token = Token::Div;
                return;
            }

            // 8) Multi-character operators
            match (ch, self.peek_char()) {
                ('=', Some('=')) => { self.next_char(); self.token = Token::Eq;  return }
                ('!', Some('=')) => { self.next_char(); self.token = Token::Ne;  return }
                ('<', Some('=')) => { self.next_char(); self.token = Token::Le;  return }
                ('>', Some('=')) => { self.next_char(); self.token = Token::Ge;  return }
                ('<', Some('<')) => { self.next_char(); self.token = Token::Shl; return }
                ('>', Some('>')) => { self.next_char(); self.token = Token::Shr; return }
                ('|', Some('|')) => { self.next_char(); self.token = Token::Lor; return }
                ('&', Some('&')) => { self.next_char(); self.token = Token::Lan; return }
                ('+', Some('+')) => { self.next_char(); self.token = Token::Inc; return }
                ('-', Some('-')) => { self.next_char(); self.token = Token::Dec; return }
                _ => {}
            }

            // 9) Single-character operators & punctuation
            self.token = match ch {
                '='  => Token::Assign,
                '!'  => Token::Operator('!'),
                '<'  => Token::Lt,
                '>'  => Token::Gt,
                '|'  => Token::Or,
                '&'  => Token::And,
                '+'  => Token::Add,
                '-'  => Token::Sub,
                '*'  => Token::Mul,
                '%'  => Token::Mod,
                '['  => Token::Brak,
                '?'  => Token::Cond,
                ';'|','|'{'|'}'|'('|')'|']'|':' => Token::Operator(ch),
                _    => continue, // skip unrecognized
            };
            return;
        }
    }
    fn program(&mut self) -> Result<(), String> {
        self.next();
        while self.token != Token::EOF {
            self.declaration()?;
        }
        Ok(())
    }
    fn declaration(&mut self) -> Result<(), String> {
        // --- 0) Handle “enum { … }” declarations ---
        if self.token == Token::Enum {
            self.next(); // consume `enum`
    
            // require opening `{`
            if self.token != Token::Operator('{') {
                return Err(format!("{}: expected '{{' after enum", self.line));
            }
            self.next(); // consume `{`
    
            // counter for enum values
            let mut i: i64 = 0;
    
            // loop until closing `}`
            while self.token != Token::Operator('}') {
                // 0a) identifier
                if self.token != Token::Id {
                    return Err(format!("{}: bad enum identifier", self.line));
                }
                let name = self.ident.clone();
                self.next(); // consume the identifier
    
                // 0b) optional “= number”
                if self.token == Token::Assign {
                    self.next(); // consume `=`
                    if let Token::Num(v) = self.token {
                        i = v;
                    } else {
                        return Err(format!("{}: bad enum initializer", self.line));
                    }
                    self.next(); // consume the number
                }
    
                // 0c) insert into symbol table as a numeric constant
                let hash = name.bytes()
                    .fold(0u64, |h, b| h.wrapping_mul(31).wrapping_add(b as u64));
                self.symbols.insert(
                    name.clone(),
                    Identifier {
                        token:  Token::Num(i),
                        hash,
                        name:   name.clone(),
                        class:  Class::Num,
                        type_:  Type::Int,
                        value:  i,
                        hclass: Class::Num,
                        htype:  Type::Int,
                        hval:   i,
                    },
                );
                i += 1;
    
                // 0d) consume optional comma
                if self.token == Token::Operator(',') {
                    self.next();
                }
            }
    
            // consume the `}`
            self.next();
            return Ok(());
        }
    
        // --- 1) Base type ---
        let mut ty = match self.token {
            Token::Int  => { self.next(); Type::Int },
            Token::Char => { self.next(); Type::Char },
            _ => return Err(format!("{}: expected 'int' or 'char'", self.line)),
        };
    
        // --- 2) Pointer stars ---
        while self.token == Token::Operator('*') {
            self.next();
            ty = Type::Ptr(Box::new(ty.clone()));
        }
    
        // --- 3) Identifier ---
        let name = self.ident.clone();
        if self.token != Token::Id {
            return Err(format!("{}: identifier expected", self.line));
        }
        // Insert as global by default
        self.symbols.insert(
            name.clone(),
            Identifier {
                token:  Token::Id,
                hash:   0,
                name:   name.clone(),
                class:  Class::Glo,
                type_:  ty.clone(),
                value:  self.data_ptr as i64,
                hclass: Class::Glo,
                htype:  ty.clone(),
                hval:   0,
            },
        );
        self.next();
    
        // --- 4) Function vs. Global ---
        if self.token == Token::Operator('(') {
            // 4a) consume '(' and balance nested parens
            let mut depth = 1;
            self.next(); // eat '('
            while depth > 0 {
                match self.token {
                    Token::Operator('(') => depth += 1,
                    Token::Operator(')') => depth -= 1,
                    Token::EOF => {
                        return Err(format!("{}: unexpected EOF in parameter list", self.line));
                    }
                    _ => {}
                }
                self.next();
            }
    
            // 4b) require '{'
            if self.token != Token::Operator('{') {
                return Err(format!(
                    "{}: expected '{{' after function parameters",
                    self.line
                ));
            }
            // mark as function
            if let Some(id) = self.symbols.get_mut(&name) {
                id.class = Class::Fun;
            }
            // !1) record where this function’s code will start:
            let entry_pc = self.code.len() as i64;
            if let Some(id) = self.symbols.get_mut(&name) {
                id.value = entry_pc;
            }

            // !2) emit ENT <#locals> — locals will be tracked in self.local_offset
            self.emit(Opcode::ENT);
            self.emit_operand(self.local_offset);


            // 4c) parse body, requiring a matching '}'
            self.next(); // consume '{'
            while self.token != Token::Operator('}') {
                if self.token == Token::EOF {
                    return Err(format!(
                        "{}: expected '}}' at end of function body",
                        self.line
                    ));
                }
                self.stmt()?;
            }
            // at the end of the function body, before consuming the '}'…
            self.emit(Opcode::LEV);
            
            self.next(); // consume '}'
        } else {
            // global‐variable: expect ';'
            if self.token != Token::Operator(';') {
                return Err(format!("{}: expected ';' after global", self.line));
            }
            self.next();
            // reserve storage
            let size = match ty {
                Type::Char => 1,
                _          => std::mem::size_of::<i32>() as i64,
            };
            self.data_ptr += size as usize;
        }
    
        Ok(())
    }
    //----------------------------------------------------------------------------- 
    // 8)  expr & stmt 
    //-----------------------------------------------------------------------------
/// Parse an expression with precedence-climbing (Pratt) and emit VM code.
/// - Handles prefix (unary) and primary expressions (e.g., literals, identifiers).
/// - Processes infix (binary) operators based on their precedence.
/// - Emits the appropriate VM instructions for the parsed expression.
fn expr(&mut self, level: i32) -> Result<(), String> {
    // ——— 1) PREFIX / PRIMARY ——————————————
    // We'll need to remember the type of the left‐hand side for assignment.
    let mut left_type = Type::Int;

    match self.token {
        // —— literal number —— 
        Token::Num(v) => {
            self.emit(Opcode::IMM);
            self.emit_operand(v);
            self.next();
            left_type = Type::Int;
        }

        // —— identifier or function call —— 
        Token::Id => {
            let name = self.ident.clone();
            self.next(); // consume the identifier
        
            // — extract just the bits we need, then drop the borrow immediately —
            let (sym_class, sym_val, sym_type) = {
                let entry = self.symbols.get(&name)
                    .ok_or_else(|| format!("{}: undefined identifier {}", self.line, name))?;
                (entry.class, entry.value, entry.type_.clone())
            };
            // now `entry`'s borrow is gone
        
            // remember the type for later (e.g. in store)
            left_type = sym_type.clone();
        
            // handle calls vs loads
            if self.token == Token::Operator('(') {
                // function call…
                self.next(); // consume '('
                let mut argc = 0;
                while self.token != Token::Operator(')') {
                    self.expr(Precedence::Assign as i32)?;
                    argc += 1;
                    if self.token == Token::Operator(',') {
                        self.next();
                    }
                }
                self.next(); // consume ')'
        
                // emit the right VM sequence
                match sym_class {
                    Class::Sys => self.emit_syscall(sym_val),
                    Class::Fun => {
                        self.emit(Opcode::JSR);
                        self.emit_operand(sym_val);
                    }
                    _ => return Err(format!("{}: not a function: {}", self.line, name)),
                }
                if argc > 0 {
                    self.emit(Opcode::ADJ);
                    self.emit_operand(argc);
                }
            } else {
                // variable load
                match sym_class {
                    Class::Loc => {
                        self.emit(Opcode::LEA);
                        self.emit_operand(self.local_offset - sym_val);
                    }
                    Class::Glo => {
                        self.emit(Opcode::IMM);
                        self.emit_operand(sym_val);
                    }
                    _ => {}
                }
                match sym_type {
                    Type::Char => self.emit(Opcode::LC),
                    _          => self.emit(Opcode::LI),
                }
            }
        }
        
        // —— parenthesis: grouping or cast —— 
        Token::Operator('(') => {
            self.next();
            // cast?
            if matches!(self.token, Token::Int | Token::Char) {
                // read cast‐type
                let mut cast_type = if self.token == Token::Int {
                    Type::Int
                } else {
                    Type::Char
                };
                self.next();
                // pointer stars
                while self.token == Token::Operator('*') {
                    self.next();
                    cast_type = Type::Ptr(Box::new(cast_type));
                }
                // expect ')'
                if self.token != Token::Operator(')') {
                    return Err(format!("{}: expected ')' after cast", self.line));
                }
                self.next();
                // parse the casted expression
                self.expr(Precedence::Inc as i32)?;
                left_type = cast_type;
            }
            else {
                // just a grouped sub-expression
                self.expr(Precedence::Assign as i32)?;
                if self.token != Token::Operator(')') {
                    return Err(format!("{}: expected ')'", self.line));
                }
                self.next();
            }
        }
        Token::Float(v) => {
            self.emit(Opcode::IMM);
            self.emit_operand(v.to_bits() as i64);
            self.current_type = Type::Float;    // <— mark it
            self.next();
        }
        

        // —— sizeof —— 
        Token::Sizeof => {
            self.next();
            if self.token != Token::Operator('(') {
                return Err(format!("{}: expected '(' after sizeof", self.line));
            }
            self.next();
            // compute the type’s size
            let mut sz_type = if self.token == Token::Int {
                self.next();
                Type::Int
            } else if self.token == Token::Char {
                self.next();
                Type::Char
            } else {
                return Err(format!("{}: bad sizeof type", self.line));
            };
            while self.token == Token::Operator('*') {
                self.next();
                sz_type = Type::Ptr(Box::new(sz_type));
            }
            if self.token != Token::Operator(')') {
                return Err(format!("{}: expected ')'", self.line));
            }
            self.next();

            // push the size
            let size = match sz_type {
                Type::Char      => 1,
                _               => std::mem::size_of::<i32>() as i64,
            };
            self.emit(Opcode::IMM);
            self.emit_operand(size);
            left_type = Type::Int;
        }

        // —— unary operators ——  
        Token::Operator('+')
      | Token::Operator('-')
      | Token::Operator('!')
      | Token::Operator('~')
      | Token::Inc
      | Token::Dec
      | Token::Operator('&')
      | Token::Mul => {
            let op = self.token;
            self.next();
            // for pre-inc/dec we need to handle specially, but for now:
            self.expr(Precedence::Inc as i32)?;
            match op {
                Token::Operator('+') => {} // no-op
                Token::Operator('-') => {
                    self.emit(Opcode::IMM);
                    self.emit_operand(-1);
                    self.emit(Opcode::MUL);
                }
                Token::Operator('!') => {
                    self.emit(Opcode::PSH);
                    self.emit(Opcode::IMM);
                    self.emit_operand(0);
                    self.emit(Opcode::EQ);
                }
                Token::Operator('~') => {
                    self.emit(Opcode::PSH);
                    self.emit(Opcode::IMM);
                    self.emit_operand(-1);
                    self.emit(Opcode::XOR);
                }
                Token::Operator('&') => {
                    // address-of: drop the last load
                    let last = self.code.pop().unwrap();
                    let _    = self.code.pop().unwrap();
                }
                Token::Mul => {
                    // dereference
                    if let Type::Ptr(inner) = self.current_type.clone() {
                        self.current_type = *inner;
                    } else {
                        return Err(format!("{}: bad dereference", self.line));
                    }
                    match self.current_type {
                        Type::Char => self.emit(Opcode::LC),
                        _          => self.emit(Opcode::LI),
                    }
                }
                Token::Inc | Token::Dec => {
                    // handle ++/-- here if you like…
                }
                _ => {}
            }
        }
        Token::Float(v) => {
            self.emit(Opcode::IMM);
            self.emit_operand(v.to_bits() as i64); // Store as raw bits
            self.next();
        }
        _ => {
            return Err(format!("{}: bad expression start: {:?}", self.line, self.token));
        }
    }

// ——— 2) INFIX / BINARY ——————————————
while self.token.precedence() >= level {
    let op = self.token;
    let prec = op.precedence();
    self.next();

    // … ternary and assignment cases …

    // —— normal binary —— 
    // 1) push the *left* operand (in A) onto the VM stack
    self.emit(Opcode::PSH);
    // 2) parse the right‐hand side, which will leave its value in A
    self.expr(prec + 1)?;
    // 3) now emit the operation, which will pop LHS, combine with RHS in A
    if self.current_type == Type::Float {
        // floating‐point ops
        match op {
            Token::Add => self.emit(Opcode::FADD),
            Token::Sub => self.emit(Opcode::FSUB),
            Token::Mul => self.emit(Opcode::FMUL),
            Token::Div => self.emit(Opcode::FDIV),
            _ => {} // no other float ops
        }
    } else {
        match op {
            Token::Add => self.emit(Opcode::ADD),
            Token::Sub => self.emit(Opcode::SUB),
            Token::Mul => self.emit(Opcode::MUL),
            Token::Div => self.emit(Opcode::DIV),
            Token::Mod => self.emit(Opcode::MOD),
            Token::Shl => self.emit(Opcode::SHL),
            Token::Shr => self.emit(Opcode::SHR),
            Token::Lt  => self.emit(Opcode::LT),
            Token::Gt  => self.emit(Opcode::GT),
            Token::Le  => self.emit(Opcode::LE),
            Token::Ge  => self.emit(Opcode::GE),
            Token::Eq  => self.emit(Opcode::EQ),
            Token::Ne  => self.emit(Opcode::NE),
            Token::And => self.emit(Opcode::AND),
            Token::Xor => self.emit(Opcode::XOR),
            Token::Or  => self.emit(Opcode::OR),
            Token::Lan => self.emit(Opcode::AND), // && 
            Token::Lor => self.emit(Opcode::OR),  // || 
            _ => {}
        }
}
}

    Ok(())

}
   
/// Parses a statement and emits VM code.

/// - Handles different types of statements, such as:
///   - Local variable declarations ( `int a;`).
///   - Control flow statements (`if`, `while`).
///   - Expression statements ( `a = b + c;`).
/// - Manages the scope and lifetime of local variables within functions.  
///implementation 
////// # Implementation Details
/// - Skips over local declarations inside functions.
/// - Parses and emits code for control flow constructs like `if-else` and `while`.
/// - Ensures proper handling of block scopes and braces.
fn stmt(&mut self) -> Result<(), String> {
    // —— skip local declarations inside functions —— 
    if self.token == Token::Int || self.token == Token::Char {
        // consume 'int' or 'char'
        self.next();
        // skip until the semicolon
        while self.token != Token::Operator(';') && self.token != Token::EOF {
            self.next();
        }
        // consume the ';'
        if self.token == Token::Operator(';') {
            self.next();
        }
        return Ok(());
    }

    // —— if‐else —— 
    if self.token == Token::If {
        self.next(); // eat 'if'
        // expect '('
        if self.token != Token::Operator('(') {
            return Err(format!("{}: expected '(' after if", self.line));
        }
        self.next();
        // parse condition
        self.expr(Precedence::Assign as i32)?;
        // expect ')'
        if self.token != Token::Operator(')') {
            return Err(format!("{}: expected ')'", self.line));
        }
        self.next();

        // emit BZ <offset>
        self.emit(Opcode::BZ);
        let patch_if = self.code.len();
        self.emit_operand(0);

        // then‐branch
        self.stmt()?;

        // optional else
        if self.token == Token::Else {
            // patch off‐to‐after‐then
            self.code[patch_if] = self.code.len() as i64;

            // emit JMP <offset>
            self.emit(Opcode::JMP);
            let patch_else = self.code.len();
            self.emit_operand(0);

            self.next(); // eat 'else'
            self.stmt()?; 

            // patch JMP to after‐else
            self.code[patch_else] = self.code.len() as i64;
        } else {
            // no else: patch BZ to after‐then
            self.code[patch_if] = self.code.len() as i64;
        }
        return Ok(());
    }

    // —— while loop —— 
    if self.token == Token::While {
        self.next(); // eat 'while'
        let loop_start = self.code.len();

        if self.token != Token::Operator('(') {
            return Err(format!("{}: expected '(' after while", self.line));
        }
        self.next();
        self.expr(Precedence::Assign as i32)?;
        if self.token != Token::Operator(')') {
            return Err(format!("{}: expected ')'", self.line));
        }
        self.next();

        // emit BZ <break>
        self.emit(Opcode::BZ);
        let patch_break = self.code.len();
        self.emit_operand(0);

        // loop body
        self.stmt()?;

        // jump back to top
        self.emit(Opcode::JMP);
        self.emit_operand(loop_start as i64);

        // patch break target
        self.code[patch_break] = self.code.len() as i64;
        return Ok(());
    }

    // —— return —— 
    if self.token == Token::Return {
        self.next(); // eat 'return'
        // optional expression
        if self.token != Token::Operator(';') {
            self.expr(Precedence::Assign as i32)?;
        }
        // expect ';'
        if self.token != Token::Operator(';') {
            return Err(format!("{}: expected ';' after return", self.line));
        }
        self.next();
        self.emit(Opcode::LEV);
        return Ok(());
    }

    // —— block —— 
    if self.token == Token::Operator('{') {
        self.next(); // eat '{'
        while self.token != Token::Operator('}') && self.token != Token::EOF {
            self.stmt()?;
        }
        if self.token == Token::Operator('}') {
            self.next();
        }
        return Ok(());
    }

    // —— empty statement —— 
    if self.token == Token::Operator(';') {
        self.next();
        return Ok(());
    }

    // —— expression statement —— 
    self.expr(Precedence::Assign as i32)?;
    if self.token != Token::Operator(';') {
        return Err(format!("{}: expected ';'", self.line));
    }
    self.next();
    Ok(())
}
    //----------------------------------------------------------------------------- 
    // 9) Code emission i removed emisson and took it to impl compiler & simple VM
    //-----------------------------------------------------------------------------

    fn print_assembly(&self) {
        for w in &self.code {
            println!("{}", w);
        }
    }

fn run(&mut self) {
    // A “register” for most operations
    let mut a: i64 = 0;
    // Cycle counter for debugging
    let mut cycle: usize = 0;
    // We’ll treat `self.stack` as our VM stack (push/pop at the end).
    // A local frame pointer; start at “bottom” of the stack
    let mut bp: usize = 0;

    while self.pc < self.code.len() {
        // Fetch
        let op = self.code[self.pc];
        self.pc += 1;
        cycle += 1;

        // Optional trace
        if self.debug {
            println!("{}:\t{:?}\tA={}\tSTACK={:?}",
                     cycle,
                     unsafe { std::mem::transmute::<i64, Opcode>(op) },
                     a,
                     self.stack);
        }

        // Dispatch
        if op == Opcode::LEA as i64 {
            let offset = self.code[self.pc] as usize;
            self.pc += 1;
            // address = bp + offset
            a = (bp + offset) as i64;
        }
        else if op == Opcode::IMM as i64 {
            a = self.code[self.pc];
            self.pc += 1;
        }
        else if op == Opcode::JMP as i64 {
            let addr = self.code[self.pc] as usize;
            self.pc = addr;
        }
        else if op == Opcode::JSR as i64 {
            let addr = self.code[self.pc] as usize;
            self.pc += 1;
            // push return address
            self.stack.push((self.pc) as i64);
            // jump
            self.pc = addr;
        }
        else if op == Opcode::BZ as i64 {
            let addr = self.code[self.pc] as usize;
            self.pc += 1;
            if a == 0 {
                self.pc = addr;
            }
        }
        else if op == Opcode::BNZ as i64 {
            let addr = self.code[self.pc] as usize;
            self.pc += 1;
            if a != 0 {
                self.pc = addr;
            }
        }
        else if op == Opcode::ENT as i64 {
            // enter subroutine: push old bp, set bp = current sp, allocate locals
            let n_locals = self.code[self.pc] as usize;
            self.pc += 1;
            self.stack.push(bp as i64);
            bp = self.stack.len();
            // push space for n_locals (initialized to 0)
            for _ in 0..n_locals {
                self.stack.push(0);
            }
        }
        else if op == Opcode::ADJ as i64 {
            // stack adjust: pop n values
            let n = self.code[self.pc] as usize;
            self.pc += 1;
            for _ in 0..n {
                self.stack.pop();
            }
        }
        else if op == Opcode::LEV as i64 {
            // if there's no call‐frame, this is the return from `main`
            if self.stack.len() < 2 {
                println!("Program returned: {}", a);
                return;
            }
            // otherwise, do a normal subroutine leave
            self.stack.truncate(bp);
            let old_bp = self.stack.pop().unwrap() as usize;
            let ret_pc = self.stack.pop().unwrap() as usize;
            bp = old_bp;
            self.pc = ret_pc;
        }
        
        else if op == Opcode::LI as i64 {
            // load int (4 bytes little-endian)
            let addr = a as usize;
            let bytes = &self.data[addr .. addr + 4];
            let val = i32::from_le_bytes(bytes.try_into().unwrap()) as i64;
            a = val;
        }
        else if op == Opcode::LC as i64 {
            // load char
            let addr = a as usize;
            a = self.data[addr] as i8 as i64;
        }
        else if op == Opcode::SI as i64 {
            // store int
            let addr = self.stack.pop().unwrap() as usize;
            let val = a as i32;
            self.data[addr .. addr + 4]
                .copy_from_slice(&val.to_le_bytes());
        }
        else if op == Opcode::SC as i64 {
            // store char
            let addr = self.stack.pop().unwrap() as usize;
            self.data[addr] = a as u8;
        }
        else if op == Opcode::PSH as i64 {
            // push the register A onto stack
            self.stack.push(a);
        }
        else if op == Opcode::OR  as i64 { a = self.stack.pop().unwrap() |  a; }
        else if op == Opcode::XOR as i64 { a = self.stack.pop().unwrap() ^  a; }
        else if op == Opcode::AND as i64 { a = self.stack.pop().unwrap() &  a; }
        else if op == Opcode::EQ  as i64 { a = if self.stack.pop().unwrap() == a {1} else {0}; }
        else if op == Opcode::NE  as i64 { a = if self.stack.pop().unwrap() != a {1} else {0}; }
        else if op == Opcode::LT  as i64 { a = if self.stack.pop().unwrap() <  a {1} else {0}; }
        else if op == Opcode::GT  as i64 { a = if self.stack.pop().unwrap() >  a {1} else {0}; }
        else if op == Opcode::LE  as i64 { a = if self.stack.pop().unwrap() <= a {1} else {0}; }
        else if op == Opcode::GE  as i64 { a = if self.stack.pop().unwrap() >= a {1} else {0}; }
        else if op == Opcode::SHL as i64 { a = self.stack.pop().unwrap() << a; }
        else if op == Opcode::SHR as i64 { a = self.stack.pop().unwrap() >> a; }
        else if op == Opcode::ADD as i64 { a = self.stack.pop().unwrap() +  a; }
        else if op == Opcode::SUB as i64 { a = self.stack.pop().unwrap() -  a; }
        else if op == Opcode::MUL as i64 { a = self.stack.pop().unwrap() *  a; }
        else if op == Opcode::DIV as i64 { a = self.stack.pop().unwrap() /  a; }
        else if op == Opcode::MOD as i64 { a = self.stack.pop().unwrap() %  a; }
    
        else if op == Opcode::FADD as i64 {
            // pop left operand bits, reinterpret as f64
            let lhs_bits = self.stack.pop().unwrap() as u64;
            let lhs = f64::from_bits(lhs_bits);
            // `a` currently holds the right operand bits
            let rhs = f64::from_bits(a as u64);
            // perform the add
            let res = lhs + rhs;
            // store result back into A as raw bits
            a = res.to_bits() as i64;
        }
        else if op == Opcode::FSUB as i64 {
            let lhs_bits = self.stack.pop().unwrap() as u64;
            let lhs = f64::from_bits(lhs_bits);
            let rhs = f64::from_bits(a as u64);
            let res = lhs - rhs;
            a = res.to_bits() as i64;
        }
        else if op == Opcode::FMUL as i64 {
            let lhs_bits = self.stack.pop().unwrap() as u64;
            let lhs = f64::from_bits(lhs_bits);
            let rhs = f64::from_bits(a as u64);
            let res = lhs * rhs;
            a = res.to_bits() as i64;
        }
        else if op == Opcode::FDIV as i64 {
            let lhs_bits = self.stack.pop().unwrap() as u64;
            let lhs = f64::from_bits(lhs_bits);
            let rhs = f64::from_bits(a as u64);
            let res = lhs / rhs;
            a = res.to_bits() as i64;
        }
    // ——— library calls (std‐only) ———

        else if op == Opcode::OPEN as i64 {
            // stack: …, ptr_to_path, _flags
            let _flags = self.stack.pop().unwrap();       // ignore flags
            let ptr    = self.stack.pop().unwrap() as usize;
            // Read a nul-terminated string from data_pool
            let slice = &self.data_pool[ptr..];
            let end = slice.iter().position(|&b| b == 0).unwrap_or(slice.len());
         let path  = String::from_utf8_lossy(&slice[..end]).into_owned();
            // Open read-only
            let fd = match std::fs::File::open(&path) {
                Ok(f) => {
                    let id = self.fd_table.len() as i64;
                    self.fd_table.insert(id, f);
                    id
                }
                Err(_) => -1,
            };
            a = fd;
        }

        else if op == Opcode::READ as i64 {
            // stack: …, fd, buf_ptr, count
            let count  = self.stack.pop().unwrap() as usize;
            let bufptr = self.stack.pop().unwrap() as usize;
            let fd     = self.stack.pop().unwrap();
            let mut buf = vec![0u8; count];
            let n = self.fd_table
                .get_mut(&fd)
                .and_then(|file| file.read(&mut buf).ok())
                .unwrap_or(0);
            // Copy into data_pool
            self.data[bufptr..bufptr + n].copy_from_slice(&buf[..n]);
            a = n as i64;
        }

        else if op == Opcode::CLOS as i64 {
            // stack: …, fd
            let fd = self.stack.pop().unwrap();
            self.fd_table.remove(&fd);
            a = 0;
        }

        else if op == Opcode::PRTF as i64 {
            // stack: …, arg1, ptr_to_fmt
            let arg1 = self.stack.pop().unwrap();
            let fmt_ptr = self.stack.pop().unwrap() as usize;
            // read the C string
            let slice = &self.data_pool[fmt_ptr..];
            let end = slice.iter().position(|&b| b == 0).unwrap_or(slice.len());
            let fmt   = String::from_utf8_lossy(&slice[..end]);
            // replace all "%d" in fmt with the integer
            let out = fmt.replace("%d", &arg1.to_string());
            print!("{}", out);
            a = 0;
        }
        
        

        else if op == Opcode::MALC as i64 {
            // stack: …, size
            let size = self.stack.pop().unwrap() as usize;
            let ptr  = self.heap_alloc.len();
            self.heap_alloc.resize(ptr + size, 0);
            a = ptr as i64;
        }

        else if op == Opcode::FREE as i64 {
            // stack: …, ptr
            let _ptr = self.stack.pop().unwrap() as usize;
            // no-op for our bump allocator
            a = 0;
        }

        else if op == Opcode::MSET as i64 {
            // stack: …, value, size, ptr
            let size = self.stack.pop().unwrap() as usize;
            let val  = self.stack.pop().unwrap() as u8;
            let ptr  = self.stack.pop().unwrap() as usize;
            self.data_pool[ptr..ptr + size].fill(val);
            a = ptr as i64;
        }

        else if op == Opcode::MCMP as i64 {
            // stack: …, size, p2, p1
            let size = self.stack.pop().unwrap() as usize;
            let p2   = self.stack.pop().unwrap() as usize;
            let p1   = self.stack.pop().unwrap() as usize;
            let cmp = self.data_pool[p1..p1 + size]
                .cmp(&self.data_pool[p2..p2 + size]);
            a = cmp as i64;
        }

        // final exit
        else if op == Opcode::EXIT as i64 {
            if self.debug {
                println!("exit({}) cycle={}", self.stack.pop().unwrap(), cycle);
            }
            return;
        } else {
            panic!("Unknown instruction: {}", op);
        }

        }
    }

}

//----------------------------------------------------------------------------- 
// 10) `main()`
//-----------------------------------------------------------------------------
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
        // **jump to main**, not to 0
        if let Some(main_id) = compiler.symbols.get("main") {
            compiler.pc = main_id.value as usize;
        }
        // right before `compiler.run();` do:
        compiler.pc = compiler.symbols.get("main").unwrap().value as usize;

        compiler.run(); 
       }
}
