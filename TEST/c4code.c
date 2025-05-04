// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long // Define int as long long for 64-bit support to avoid  truncation, ensures safe pointer arithmetic.

char *p, *lp, // current position in source code, p: current position in source code, lp: last position
     *data;   // data/bss pointer

int *e, *le,  // e: current position in emitted code, le: last emitted code position
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

/*tokens and classes (operators last and in precedence order)
Tokens start at 128 to avoid conflicts with ASCII characters which is within the range  (0-127)
enum is user-defined data type that assigns named integer constants to a set of values. 
It improves code readability and maintainability by giving meaningful names to numeric values.
the types of token identifien in this codes are  Literals and Identifiers, Keywords, Operators
These tokens are reserved in any compiler or programming language because they define the fundamental
building blocks of parsing and interpreting source code.*/

enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

/* opcodes :An opcode is a numeric instruction that tells the computer or virtual machine what operation to perform.
These opcodes represent basic operations like arithmetic, memory access, control flow, and system calls.*/
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

/* types of data which important for a compiler to know in order to understand data types. 
which is a fundamental to how a program's memory is managed and how operations are performed on the data. */
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };


//Function to read the next token from source code
void next()
{
  char *pp; //Pointer to hold the start of an identifier or string


  //Main loop processes the input until the end of input 
  while (tk = *p) {
    ++p; //pre increment to Move to the next character in the source code
    if (tk == '\n') {// Check for a newline character
      if (src) {// If we are in source mode
        printf("%lld: %.*s", line, p - lp, lp);// Print the current line number and the source code line
        lp = p; // Update the last position pointer to the current position
        
        //Process each instruction 
        while (le < e) {// Loop through each instruction, where 'le' is the instruction pointer and 'e' is the end limit
        // Print the current instruction with formatting, based on an array of instruction names
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
                           
                           
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");// 'ADJ' is a specific instruction code or identifier, and based on 
          //the comparison, the code checks if the current instruction requires printing additional data.
          // If the instruction type is less than or equal to ADJ, print the value following it Otherwise, print just a newline 
          
        }
      }
      ++line;/*- After processing all instructions in the set we increment the 'line' variable to indicate the processing of
      the next set of instructions in the following iteration.
- This is important for keeping track of line numbers or positions in the instruction sequence for logging or debugging.
*/
    }
    else if (tk == '#') { // If the token is '#', it indicates a comment, so we skip the rest of the line
      while (*p != 0 && *p != '\n') ++p; // Skip characters until we reach the end of the line or end of input
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {       
        // If the token is an identifier (starting with a letter or underscore)
      pp = p - 1;// Store the starting pointer of the identifier
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')  // Read the identifier until a non-identifier character is found

        tk = tk * 147 + *p++; //Hashing the identifier 
      tk = (tk << 6) + (p - pp);// Finalize the token value
      id = sym;// Start searching in the symbol table
     
      while (id[Tk]) {        // Check if the identifier already exists in the symbol table
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; // If found, set tk to the existing token }
        id = id + Idsz;// Move to the next entry in the symbol table
      }
      
    // If not found, add the new identifier to the symbol table
      id[Name] = (int)pp;// Store the name of the identifier
      id[Hash] = tk;// Store the hash value
      tk = id[Tk] = Id;// Assign a new token value
      return;// Exit the function
      
    }}
    else if (tk >= '0' && tk <= '9') {// If the token is a number (digit)
      if (ival = tk - '0') { // If it's a regular number
          while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0';// Convert to integer
          }
        
      else if (*p == 'x' || *p == 'X') {// If it's a hexadecimal number
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0); // Convert to integer
      }
      else {   // If it's an octal number
          while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0';// Convert to integer 
          }
      tk = Num; // Set token type to number (Num)
      return;
    }
    else if (tk == '/') {// Check if the current character is a division operator
      if (*p == '/') {// If the next character is also a '/', it indicates a comment
        ++p;// Move past the comment character
        // Skip the rest of the line until a newline is found
        while (*p != 0 && *p != '\n') ++p;// Continue moving until the end of the line or end of the string
      }
      else {// If the next character is not a '/', it is a division operator
        tk = Div;// Set the token to represent division
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {// Check for string or character literals
      pp = data;// Save the current position in the data buffer
      while (*p != 0 && *p != tk) { // Read characters until the closing quote is found
        if ((ival = *p++) == '\\') {// Check for escape sequences
          if ((ival = *p++) == 'n') ival = '\n';// Convert '\n' to a newline character
        }
        if (tk == '"') *data++ = ival;// If it's a string, store the character in the data buffe
      }
      ++p; // Move past the closing quote
      if (tk == '"') ival = (int)pp; // If it's a string, set ival to the start of the string
      else tk = Num;// If it's a character literal, set the token to a numeric type
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }// Check for the equality operator If the next character is also '=', it indicates a comparison
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }// If it's '++', set token type to Inc Otherwise, it's a regular addition operator
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }// If it's '--', set token type to Dec Otherwise, it's a regular subtraction operator
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }  // If the token is '!', check if it's '!=' (not equal)  // If it's '!=', set token type to Ne
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; } // If the token is '%', set token type to Mod (modulo operator)
    else if (tk == '*') { tk = Mul; return; }// If the token is '*', set token type to Mul (multiplication operator)
    else if (tk == '[') { tk = Brak; return; }  // If the token is '[', set token type to Brak (array or bracket)
    else if (tk == '?') { tk = Cond; return; }// If the token is '?', set token type to Cond (conditional operator)
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

void expr(int lev)
{
  int t, *d; // t is a temporary storage for types, d stores identifier information

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }   // Check for unexpected end-of-file during parsing and Terminates if eof is reached
    
    
  else if (tk == Num) {  // If the token is a number, generate code to load it as an immediate value 
      *++e = IMM; /* Store opcode to load an immediate value. ++e: Pre-increment the pointer e
    Moves e to the next memory location before evaluating *e e it is important becuase it ensures that each emitted 
    instruction or value is stored sequentially in memory, maintaining proper code structure 
    and execution order within the compiler's virtual machine. */
      *++e = ival; // Store the numeric value in emitted code
      next(); // Move to the next token
      ty = INT; // Mark expression type as integer
      }
     
  else if (tk == '"') {  // If the token is a string literal, store its address as an immediate value
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();// Handle concatenated string literals
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); //Aligning the data pointer to a word boundary ensures it is a multiple of sizeof(int) to avoid misalignment issues, with the bitwise operation & -sizeof(int) rounding it down to the nearest properly aligned address.
    ty = PTR;// Strings are pointers in memory
  }
  else if (tk == Sizeof) {  // Handle the sizeof operator, which determines the memory size of a type
    next(); // Move to the next token
    if (tk == '(') next();  // Expect an opening parenthesis if it is next
    else { printf("%d: open paren expected in sizeof\n", line); exit(-1); } //error message indicating that an open parenthesis was expected, and the program exits with an error code -1.
    ty = INT; //The variable ty is initialized to INT, meaning it assumes the type to be an int by default.
    if (tk == Int) next();// If the next token is 'int', move to the next token
    else if (tk == Char) { next(); ty = CHAR; }//If the current token is Char, it moves past it and sets ty to CHAR, meaning the type is now char.
    while (tk == Mul) { next(); ty = ty + PTR; }//This loop checks if the token is a pointer (*). If so, it moves past the token and adjusts ty by adding PTR to it, effectively increasing the level of indirection for pointers
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }//The next token should be a closing parenthesis ). If it is, the code moves past the closing parenthesis if not print error msg
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);//The pointer e is again incremented and the size of the type (char or int) is stored at that position. If ty is CHAR, it stores sizeof(char); otherwise, it stores sizeof(int).
    ty = INT;//After processing the sizeof operator, the type (ty) is reset back to INT
  }
  else if (tk == Id) {//checks if the current token (tk) is an identifier (Id)
    d = id; next();//If it is it assigns the identifier's information to the d 
    if (tk == '(') {
      next();//If the next token is an opening parenthesis it advances to the next token.
      t = 0;//Initializes t to 0. This variable will be used to count the number of arguments passed to the function.
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }// loop processes each argument inside the parentheses. It evaluates each expression (using the expr(Assign) function, to processes assignment expressions),
      //which then pushes the result onto the evaluation stack (PSH), and increments the argument count t. If there is a comma , (indicating another argument), it moves to the next token.
      next();//Moves past the closing parenthesis ).
      if (d[Class] == Sys) *++e = d[Val];//If the identifier d is a system function (Sys), it places the value of the function into the evaluation code.
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }//If d represents a function (Fun), it generates code for a function call by pushing a JSR (jump to subroutine) instruction, followed by the function's address or value (d[Val]).
      else { printf("%d: bad function call\n", line); exit(-1); }//If d is neither a system function nor a regular function, it prints an error message indicating a bad function call and exits the program
      if (t) { *++e = ADJ; *++e = t; }
      ty = d[Type];//The variable ty (which tracks the type of the identifier) is set to the type of d (the identifier).

    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }//If d is a constant number (Num), it pushes the constant value (d[Val]) onto the evaluation stack as an immediate value (IMM) and sets ty to INT (integer type).
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }//If d is a local variable (Loc), it generates code to load the address of the local variable (LEA) by calculating the difference between the current location (loc) and the variable's value (d[Val]).
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }//If d is a global variable (Glo), it generates code to load the immediate value (IMM) corresponding to the global variable's value (d[Val]).
      else { printf("%d: undefined variable\n", line); exit(-1); }//If d is not a recognized class (not a local, global, or number), it prints an error message indicating that the variable is undefined and exits 
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;//checks the type of d (d[Type]), and if it's CHAR, it loads the constant for a char type using LC. If it's not CHAR, it loads the constant for an int type using LI. The result is pushed onto the evaluation stack.
    }
  }
  else if (tk == '(') {// if the current token (tk) is an opening parenthesis (. Parentheses often indicate an expression or a type cast in C-like languages.
    next();
    if (tk == Int || tk == Char) {//checks if the next token is a type (Int or Char). This is used for type casting. In C-like languages, type casting often happens within parentheses, such as (int) or (char)
      t = (tk == Int) ? INT : CHAR; next();// assigns the value INT to t if the current token is Int, or CHAR if the current token is Char. After that, next() is called to move to the next token, which should be the expression inside the parentheses.
      while (tk == Mul) { next(); t = t + PTR; }//. If the token is the dereference operator (*), it indicates a pointer, so the type (t) is adjusted by adding PTR. The next() function continues to consume * tokens if there are multiple pointer levels (
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }//This checks if the current token is a closing parenthesis ), which marks the end of a type cast expression. If it's not a closing parenthesis, an error message is printed
      expr(Inc);// calls the expr function to evaluate an expression with increment precedence.
      ty = t;//After evaluating the expression, the resulting type t (the cast type) is assigned to the variable ty, which stores the type of the expression.
    }
    else {//If the token wasn't a type (Int or Char), this part handles general expressions inside parentheses. It parses the expression using expr(Assign)
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }// it checks for a closing parenthesis and advances the token if it's correct. Otherwise, it prints an error message and exits.
    }
  }
  else if (tk == Mul) {//Checks if the current token is *, indicating dereferencing.
    next(); expr(Inc);//Advances to the next token and parses the expression with increment precedence. This ensures the dereferencing is done on a valid operand.
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }//if the type of the expression is a pointer type (ty > INT). If it is, it adjusts the type by subtracting PTR, indicating the dereferencing of a pointer. If it's not a pointer, it prints an error message and exits.
    *++e = (ty == CHAR) ? LC : LI;//This adds the corresponding intermediate code operation based on the type. If the type is CHAR, it loads a character (LC), otherwise it loads an integer (LI).
  }
  else if (tk == And) {//This checks for the address-of operator (&), which is used to obtain the address of a variable.
    next(); expr(Inc);//Advances to the next token and evaluates the expression with increment precedence.
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }//This checks if the current expression is a valid lvalue (LC or LI). If the expression is not a valid lvalue, it prints an error and exits.
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }/*This handles the logical NOT operator.
  It negates the expression's result.Advances to the next token and evaluates the expression with increment precedence.Pushes the result of the logical NOT operation. It compares the value to 0 (EQ), effectively converting it into a boolean (0 or 1).*/
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }/*This handles the bitwise NOT operator (~), which inverts all bits of the operand.
Similar to the logical case, advances to the next token and evaluates the expression.Pushes -1 onto the stack and XORs the result, effectively flipping all the bits of the expression.*/


  else if (tk == Add) { next(); expr(Inc); ty = INT; }//addition operator (+). This operator is straightforward and just advances to the next token, evaluates the expression, and sets the type to INT
  else if (tk == Sub) {
      /*this block handles the subtraction operator (-). It checks if the next token (tk) is a number (Num).If the token is a number, the code loads the negative value of ival (an integer) 
      into e.If it's not a number, it pushes -1 to the stack, evaluates the expression with expr(Inc)  and then multiplies the result by -1. ty is set to INT */
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {/*This block handles the pre-increment (++) or pre-decrement (--) operators First, the current token is saved as t, and the expr(Inc) function is called
  to evaluate the expression.It then checks if the value in e is a valid lvalue (LC or LI). If it's not, an error is thrown.Next, the code prepares for the increment/decrement
  operation by pushing the appropriate values onto the stack (IMM).The type is checked: if it's a CHAR, it performs the operation using SC (store char), otherwise SI (store int).*/
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    if (tk == Assign) {//If the token is Assign, the code checks whether the left operand is a valid lvalue (LC or LI).It then evaluates the right-hand side expression (expr(Assign)).
    //the result using either SC or SI depending on whether the type is CHAR or not. which is necessary for handling variable assignments, ensuring that only valid lvalues are assigned to.
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    
    //This block handles the ternary conditional operator (?:)
    else if (tk == Cond) {
        //The code processes the condition (expr(Assign)), followed by checking the : token.If valid, it jumps to the correct branches of the conditional expression using BZ (branch if zero) and JMP (unconditional jump). then the The labels are updated to mark where the conditional branches should go.
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    
    /*These blocks handle logical OR (||) and logical AND (&&).For each operator, the expression is evaluated, and the result is conditionally
    branched using BNZ (branch if not zero) or BZ (branch if zero).The result type is set to INT as these are boolean operations.*/
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
   
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }// Bitwise OR operator, evaluates the XOR of operands. Evaluate the left operand of the OR operation Perform the bitwise OR operation sace result as int The result is an integer
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }// Bitwise XOR operator, evaluates the AND of operands. Evaluate the left operand of the XOR operation Perform the bitwise XOR operation sace result as int The result is an integer
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }//// Bitwise AND operator, compares if two values are equal or not.  Evaluate the left operand of the AND operation Perform the bitwise AND operation The result is an integer
   
   
    /*These blocks handle relational operators such as ==, !=, <, >, <=, and >=. Ny having The corresponding relational expression is evaluated and the operation is performed The type is set to INT as relational operators return a boolean value.
    These operations are fundamental in comparing values, and their result is always a boolean value (0 or 1).*/
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    // Bitwise shift left and  Bitwise shift right operator, shifts the left operand to the left by the right operand.  Evaluate the left operand of the shift-left operation perform the corresponding shift to the left or right The result is an integer
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    
    else if (tk == Add) {  // Addition operator, adds two operands together.

      next(); *++e = PSH; expr(Mul);// Evaluate the left operand of the addition
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; } // Multiply for pointer types  }
      *++e = ADD;//perforn addition
    }
    else if (tk == Sub) {  // Subtraction operator, subtracts two operands.

      next(); *++e = PSH; expr(Mul);// Evaluate the left operand of the subtraction
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }// Perform division if types are compatible & set the result type to integer
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; } // Perform subtraction for pointer types
      else *++e = SUB; // Perform simple subtraction
    }
    
    //these blocks handle basic arithmetic (+, -, *, /, %) and bitwise operations (&, |, ^, <<, >>).For each operator, the expression is evaluated, and the appropriate operation is performed. Some operators
    //(like Add and Sub) require additional handling for pointer types. The type is updated to INT after operations like multiplication and division to ensure type consistency.
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    
    
    
    
    
    else if (tk == Inc || tk == Dec) {  // Post-increment or post-decrement operator.
      if (*e == LC) { *e = PSH; *++e = LC; }// Push the value of the current location onto the stack and  Load the address of the current location (LC), incrementing e to the next location on the stack
      else if (*e == LI) { *e = PSH; *++e = LI; }// Push the value of the current location onto the stack and load the value at the current location (LI), incrementing e
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }// If neither LC nor LI, print an error and exit the program
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB; // If it's an increment, push the ADD operation; if it's a decrement, push the SUB operation
      *++e = (ty == CHAR) ? SC : SI;// If the type is CHAR, push SC (store character), else push SI (store integer)
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char); // Push the size of the type (either int or char) based on the type (ty)
      *++e = (tk == Inc) ? SUB : ADD;// If it's increment, subtract; if it's decrement, add 
      next();// Move to the next token in the sequence
    }
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);// Move to the next token push vale to stack and then evaluates expression
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }// If the token is a closing bracket, move to the next token else print error and exit
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }//// Push a value to the stack Indicate an immediate value and Push the size of an integer (in bytes)
      //and finally Push the multiplication operation to multiply with the size of the typ (constant) For an array or pointer type, the offset is computed by multiplying the size of the type (integer in this case) and adding it to the address.
     
     
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }// If t is less than PTR, it’s an error and the program exits
      *++e = ADD;//The address computation for array indexing is completed by adding the computed offset to the base addr
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;//This line adjusts the load operation for the correct type based on whether the result is CHAR or another type.
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }//print error and exit
  }
}

void stmt() // function that parses and handle statments in source code, void mean there is no return
{
  int *a, *b; // store address for flow control like if and while 

  if (tk == If) { //if the token tk is if statement 
    next(); //move to the next token inside the source code, it is curcial to move the past if and start parsing the condition 
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); //call expr function to parse condition inside if statement, it should be evaluated with assignment precedence
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // assign the value of BZ to memory allocation that e now points to 
    stmt(); //recursive call to parse the body of if. It will be executed if the condition is true 
    if (tk == Else) {
      *b = (int)(e + 3); *++e = JMP; b = ++e; // pointer b is assigned to value of e+3 and cast to an integer, *e++=JMP it increment e first then JMP is stored in new position of e 
      //b = ++e, updating b to new e 
      next(); // go to next token
      stmt(); //parse and handle the next statement 
    }
    *b = (int)(e + 1); // update address in JMP to current position + 1 and cast it to integer 
  }
  else if (tk == While) { //check if current token is a while statement 
    next(); //move to next token 
    a = e + 1; // address where the loop will jump back to after each iteration 
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // *++e = BZ, increment the pointer e first, then store BZ in new positon of e, b= ++e, increment e again and store new e in b
    stmt();//parse and handle the next statement 
    *++e = JMP; *++e = (int)a; //*++e = JMP, increment pointer e, then store JMP in new positon of e, *++e = (int)a, increment pointer e again then assign a to pointer e and cast to int 
    *b = (int)(e + 1); //update address in JMP to current position + 1 and cast it to integer 
  }
  else if (tk == Return) { //check the current toke is a Reutrn statement 
    next(); //move to next token 
    if (tk != ';') expr(Assign); //if tk is not equal to ; then it calls expr function to parse the return value 
    *++e = LEV; // increment pointer e then store LEV in the new position of e 
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); } //if tk is equal to ;, error will occur and a message will show 
  }
  else if (tk == '{') { //if tk equals to open curl bracket, means block of statements is starting 
    next(); //move to next token 
    while (tk != '}') stmt(); //process each statement till it reach closing curl bracket 
    next(); //move to next token 
  }
  else if (tk == ';') { //if the token equal to ; means an empty statement, move to next token 
    next();
  }
  else {
    expr(Assign); //parse expression 
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  } // if token equals to ; move to next token, then prints error messages 
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain; //poolsz, store the size of memory pools 
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps
  int is_comp_main=1;
  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; } //if the flag was -s set source mode  
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; } //if the flag was -d set debug mode 
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; } // if no input, print usage and exit 

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; } //open the input file in argv[0], if failed print error and exit 
// Memory allocation: Allocating memory for sym, e, data, and sp if any allocation failed print error and exit 
  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  //initialize allocated memory to zero
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);
// adding C keyword and library functions to symbol table 
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; } //allocate memory for source code
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; } //read source file into memory 
  p[i] = 0;
  close(fd); //close file 

  // parse declarations
  line = 1; //initialize line counter to 1 (to indicate error occurred)
  next(); //move to next token
  while (tk) {
    bt = INT; // basetype
    if (tk == Int) next(); //check if the current token is int
    else if (tk == Char) { next(); bt = CHAR; } // if current token is char, then set the basetype bt to CHAR
    else if (tk == Enum) {
        //parse enum declarations
      next(); //move to token
      if (tk != '{') next(); // check if token is not open curly brace
      if (tk == '{') { //expecting curly brace 
        next();
        i = 0; //initialize counter i to zero
        while (tk != '}') { //loop continue until closing curly brace is found
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; } //check if the token is not identifier, if not will print error message
          next();
          if (tk == Assign) { //check if the token is assignment operatot
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; } //check if the token is a number, if not print error message
            i = ival; //set i to numeric value of ival, which allows the enum to have explicity set
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++; //assign properties to current enum
          if (tk == ',') next(); //check if token is comma, if yes move to next token
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') { //keep parsing till ; or } is found 
      ty = bt; //set tp to base type bt
      while (tk == Mul) { next(); ty = ty + PTR; } //for each * PTR is added to ty 
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; } // if there is no identifier after the type print error messaage and exit 
      if (id[Class] && !is_comp_main) { printf("%d: duplicate global definition\n", line); return -1; } //if identifier is already exit print error message and exit
      next();
      id[Type] = ty; //store ty in entry for the identifier
      if (tk == '(') { // Parse function declarations
        id[Class] = Fun; // mark the identifier as a function 
        id[Val] = (int)(e + 1); //store function entry point e+1
        next(); i = 0;
        while (tk != ')') { //keep processing till } is found
          ty = INT;
          //if the parameter has explicit type (int or char) set ty
          if (tk == Int) next(); 
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; } //handle pointer types
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; } //if parameter do not have identifier print error message
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; } // if it's already declared print error message
          /*Backup old class/type/value (HClass, HType, HVal) before modifying.
            Mark it as a local variable (Loc).
            Store its type (ty).
            Assign a parameter index (i++)*/
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next(); // , to handle multiple parameter
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; } // if { is missing print error message
        loc = ++i; //store local variable 
        next();
        while (tk == Int || tk == Char) { //while there is a declaration keep processing
          bt = (tk == Int) ? INT : CHAR; //determine the base type
          next();
          while (tk != ';') { //parse all variables
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; } //handle pointers
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; } //check if there is identifier
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; } //check for duplicates
           //save old class, type and value and assign the new local variables
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next(); //move to next or consume ,
          }
          next();
        }
        *++e = ENT; *++e = i - loc; //generate function entry with local variable count
        while (tk != '}') stmt(); //parse all functions till } is found
        *++e = LEV;
        //Restore previous state by unwinding local variables
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      } 
      //handle global variables 
      else {
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next(); //f there’s a comma, move to the next identifier.
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; } //check if the main is defined, if not print error and exit 
  if (src) return 0; //if -s exit without executing 

  // setup stack
  // push arguments for main onto the stack
  bp = sp = (int *)((int)sp + poolsz); //initilaize the stack pointer sp, and bp 
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp; //decrement stack pointer down in memory, store PSH opcode, save the current stack pointer in T
  *--sp = argc; //push argc count onto the stack
  *--sp = (int)argv; //push argv counter onto the stack and cast it to int
  *--sp = (int)t; // push previous stack pointer onto the stack

  // run...
  cycle = 0;
  while (1) { //begin executing instructions in loop
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    // implement simple stack based VM
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH) *--sp = a;                                         // push
//pop one value from the stack, apply the operation to a, and store the result in a
    else if (i == OR)  a = *sp++ |  a; //bitwise OR 
    else if (i == XOR) a = *sp++ ^  a; //bitwise XOR
    else if (i == AND) a = *sp++ &  a; //bitwise AND
    else if (i == EQ)  a = *sp++ == a; //equal to 
    else if (i == NE)  a = *sp++ != a; //not equal
    else if (i == LT)  a = *sp++ <  a; //less than
    else if (i == GT)  a = *sp++ >  a; //greater than
    else if (i == LE)  a = *sp++ <= a; //less than or equal
    else if (i == GE)  a = *sp++ >= a; // greater than or equal
    else if (i == SHL) a = *sp++ << a; //left shift 
    else if (i == SHR) a = *sp++ >> a; //right shift 
    else if (i == ADD) a = *sp++ +  a; //addition
    else if (i == SUB) a = *sp++ -  a; //subtraction
    else if (i == MUL) a = *sp++ *  a; //multiplication
    else if (i == DIV) a = *sp++ /  a; //division
    else if (i == MOD) a = *sp++ %  a; //modulo 
// implement basic functions 
    else if (i == OPEN) a = open((char *)sp[1], *sp); //call open cast to char* and flags *sp, the result stored in a 
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp); //call read cast to char* and flags *sp, the result stored in a 
    else if (i == CLOS) a = close(*sp); //call close and store the result in a 
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); } //Calls printf(), retrieving arguments from the stack
    else if (i == MALC) a = (int)malloc(*sp); //call malloc and store allocated memory address in a
    else if (i == FREE) free((void *)*sp); //call free to dealocate memory at *sp
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp); //call memset and store result in a 
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp); //call memcmp to compare two memory blocks
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; } //call printf to print exit status and cycle count
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; } // detect and print invalud instructions 
  }
}
