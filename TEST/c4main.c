int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain; //poolsz, store the size of memory pools 
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

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
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; } //if identifier is already exit print error message and exit
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