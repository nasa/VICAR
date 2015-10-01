$!****************************************************************************
$!
$! Build proc for MIPL module knuth
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:16
$!
$! Execute by entering:		$ @knuth
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module knuth ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to knuth.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("knuth.imake") .nes. ""
$   then
$      vimake knuth
$      purge knuth.bld
$   else
$      if F$SEARCH("knuth.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake knuth
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @knuth.bld "STD"
$   else
$      @knuth.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create knuth.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack knuth.com -mixed -
	-s knuth.c knuth_var.c knuth_dump.c knuth_lookup.c xknuth.c -
	   xknuth_comp.c knuth_int.h -
	-i knuth.imake -
	-t tknuth.f tknuth.imake tknuth.pdf tstknuth.pdf tstknuth.log_solos -
	-o knuth.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create knuth.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "knuth_int.h"
#include <string.h>

/*---------------------------  knuth.c  ------------------------*/
/* knuth calls a recursive formula parser, consisting of        */
/* a series of subroutines which handle successively            */
/* higher priority functional operations. At the highest        */
/* level the parenthesis routine calls level 1 to pull in       */
/* all expressions of lower level.                              */
/*                                                              */
/* The parser uses a stack-based storage system, which has      */
/* built-in monitors to convert the stack-based operations      */
/* to register-based operations.                                */
/* For example, the stack operations:                           */
/*            (push a, push b, add)                             */
/*    become the register operations:                           */
/*            (load a, add b)                                   */
/*--------------------------------------------------------------*/
/* REVISION HISTORY:                                            */
/*     SEPT 75   ALZ   ORIGINAL FORTRAN VERSION.                */
/*     JULY 91   NDR   FORTRAN VERSION SCRAPPED, WRITTEN        */
/*                      IN C FOR UNIX COMPATIBILITY, BYTE-ORDER */
/*                      INDEPENDENCE.                           */
/*     APRL 92   NDR   SEPARATED MODULES, ADDED FORSTR_DEF      */
/*                      MACRO, NEW FORTRAN CALLS.               */
/*     JULY 92   NDR   FIXED MISSING "ABS, "AMOD", "INT" CALLS  */
/*     Aug  92   SP    REMOVED +0.5 FROM XKNUTH FOR "INT" TO    */
/*                     PERFORM PROPER TRUNCATION.               */
/*     AUG  92   SP    CHANGED "NONZERO" MACRO TO CONVERT 0 TO  */
/*                     1E-6 INSTEAD OF -1E-6. (SEE IN KNUTH.H   */
/*                     ALSO CHANGED >0 TO >=0 IN "ROUND" AND    */
/*                     TRUNCATE MACROS FOR STYLE.               */
/*     SEP  92   SP    ADDED CHECK FOR ATAN2(0,0).              */
/*     SEP  92   NDR   ADDED ARGUMENT COUNT CHECK FOR ATAN2 AND */
/*                      MOD; ADDED SYNONYMS  LOG, LN, LOG10;    */
/*                     CHECK FOR TYPOS LIKE "TAN7".             */
/*     SEP  94   NDR   EXPOSED GLOBALS MADE STATIC              */
/*     FEB  07   LWK   CHANGED "SMALL" (IN KNUTH.H) FROM 1E-6   */
/*                     TO 1E-20;  ADDED CODE TO CHECK BOUNDS OF */
/*                     "INT" FUNCTION.                          */
/*--------------------------------------------------------------*/

/* macros for accessing stack and checking registers */
#define KNBUFFER           kn_buffer
#define STK_PTR(x)         (kn_stack[top_of_stack-(x)])
#define STK_VAL(x)         BUF_VAL(STK_PTR(x))
#define ACTIVE_REGISTER(x) ( STK_PTR(x)==(top_of_register) && STK_VAL(x)==0 )

/* static globals for knuth  */
static char *kn_prog,*kn_prog_string,kn_token[80],tok_type;
static float *kn_buffer;
static int kn_status,kn_uses_line_samp=0;
static int top_of_stack,top_of_register;
static int top_of_code,top_of_static;
static int kn_stack[MAXSTACK];
static int kn_load,num_arguments,alt_char_set;

/* static functions */
static void initialize_parser();
static void compile_loop();
static void check_compilation();
static void commas();
static void logical1();
static void logical2();
static void compares();
static void shifts();
static void arith1();
static void arith2();
static void exponents();
static void unary_arith();
static void transcend();
static void prefixes();
static void parentheses();
static void primitive();
static void multi();
static void check_arg_count();
static void binary();
static void unary();
static void stack_to_register();
static void push();
static void push_static();
static void push_var();
static void pop();
static void store_register();
static int load_stack();
static void print_op();
static int op_num();
static void get_token();
static void prepare_string();
static int iswhite();
static int isdelim();
static int is_in();
int kn_var_index(char* );

/* operator code lists, in order of priority */
static char LOGICAL1[]={OR,LOR,XOR,0};
static char LOGICAL2[]={AND,LAND,0};
static char COMPARES[]={LT,LE,EQ,NE,GE,GT,0};
static char SHIFTS[]={LSHF,RSHF,0};
static char ARITH1[]={ADD,SUB,0};
static char ARITH2[]={MUL,DIV,MOD,0};
static char EXPONENT[]={EXP,0};
static char UNARYARITH[]={ADD,SUB,0};
static char TRANSCEND[]={NOT,ABS,SQRT,INT,LN,LOG10,SIN,COS,TAN,ASIN,ACOS,ATAN,0};
static char PREFIXES[]={AMAX,AMIN,MOD,ATAN2,0};

/* For some Prefixes, need to check argument count */
static char PREFIX_ARG_COUNT[]={MOD,ATAN2,0};

/* List of Commutative operators -- for optimization */
static char COMMUTATIVE[]={AMAX,AMIN,ADD,MUL,AND,OR,XOR,EQ,NE,LAND,LOR,0};

/* Search lists for operators, indexed by their associated opcodes */
static char *op_name[]= {
  "x",    "x",    "x",   "x",   "x",   "**",   "ALOG10",   "ALOG", "AINT", "(",
  ",",    ")",    "x",   "x",   "x",   "x",    "SQRT", "SIN",  "COS",  "TAN",
  "MAX",  "MIN",  "MOD", "ABS", "x",   "ATAN2","ASIN", "ACOS", "ATAN", ".LT.",
  "<=",   "==",   "!=",  ">=",  ".GT.",".OR.", ".AND.",".XOR.",".NOT.","&&",
  "||",   "<<",    ">>" };

/* Alternative operator names: having secondary sets accomplishes */
/* two things: it permits alternative operator names, but also     */
/* forces searches for two-character operators before single ops   */

static char *alt_op_name[]= {
    "x",    "+",    "-",   "*",   "/",   "x",    "LOG10","LN",   "INT", "x",
    "x",    "x",    "x",   "x",   "x",   "x",    "x",    "x",   "x", "x",
    "AMAX1","AMIN1","%",   "x",   "x",   "x",    "x",    "x",   "x", "<",
    ".LE.", ".EQ.", ".NE.",".GE.",">",   "|",    "&",    "^",   "!", "x",
    "x",    "x",    "x"};

static char *alt_op_name2[]= {
    "x",    "x",    "x",   "x",   "x",   "x",    "x",    "LOG",   "x", "x",
    "x",    "x",    "x",   "x",   "x",   "x",    "x",    "x",   "x", "x",
    "x",    "x",    "AMOD","x",   "x",   "x",    "x",    "x",   "x", "x",
    "x",    "x",    "x",   "x",   "x",   "x",    "x",    "x",   "x", "x",
    "x",    "x",    "x"};
static char DELIMSTRING[]= "+-*/%(),.=&^|!<>";

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(knuth,KNUTH) (char *string, float *buf, int *status, ZFORSTR_PARAM)
#if 0
char *string;        /* input: string to parse                */
float *buf;          /* output: compiled code & registers     */
int *status;         /* output: error code                    */
#endif
{
   ZFORSTR_BLOCK
   char *c_string;
   int length;

   zsfor2len(length, string, &string, 3, 1, 1, status);
   c_string = (char *)calloc(1,(length+1));
   zsfor2c(c_string, length, string, &string, 3, 1, 1, status);

   *status = zknuth(c_string,buf);

   free (c_string);

}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

int zknuth(char* string,float *buf)
{
    initialize_parser(string,buf);
    
    if (kn_status==SUCCESS) compile_loop();

    check_compilation();

    return kn_status;
}

/************************************************************************/
/* Private Implementation                                               */
/************************************************************************/

static void initialize_parser(string,buf)
char *string;      /* input: string to parse          */
float *buf;        /* output: compiled code & registers */
{
    /* initialize globals */    
    kn_status=SUCCESS;
    KNBUFFER=buf;
  
    /* string pre-processing */
    kn_prog_string = (char *) calloc(1,strlen(string)+1);
    if (!kn_prog_string)
    {
        kn_status = BAD_FUNCTION_STRING;
        return;
    }
    strcpy(kn_prog_string,string);
    kn_prog = kn_prog_string;
    prepare_string();
    
    if (kn_status == SUCCESS)
    {
        kn_load = TRUE;
        top_of_stack = BOTTOM_OF_STACK;
        top_of_code = BOTTOM_OF_CODE;
        top_of_register = BOTTOM_OF_REGISTER;
        top_of_static = BOTTOM_OF_STATIC;
        BUF_VAL(top_of_register) = 0.0;
    
        get_token();
        
        if(!*kn_token)  /* then empty string */
            kn_status = BAD_FUNCTION_STRING;
    }
}

static void compile_loop()   /* Entry point to recursive parser section */
{
    commas();   /* lowest priority token handler */
}

static void check_compilation() /* tidies up loose ends of compiled code */
{
    free (kn_prog_string);
    
	/** Check for dangling tokens or values on stack **/ 
    if (top_of_stack-1 != BOTTOM_OF_STACK || (*kn_token) )
        kn_status=BAD_FUNCTION_STRING;/* ill-formed expression */
        
    if (top_of_code == BOTTOM_OF_CODE)
        load_stack(1);           /* just in case NOP first value */

    if (kn_status==SUCCESS)
        print_op(RTN,0);        /* append return code */

	/* warn user if LINE,SAMP..., variables referenced */
    if (kn_status==SUCCESS && kn_uses_line_samp)
	   kn_status=USES_LINE_SAMP; 

}


/*-- Beginning of the recursive parser subroutines     --*/

/* Priority level 0: Handle comma operator */
static void commas()
{
    char op;
    int temp_arg;

    if (kn_status != SUCCESS) return;
    
    logical1();
    temp_arg = 1;
    
    for(op = *kn_token; is_in(op,",");op = *kn_token)
    {
        temp_arg++;
        get_token();
        logical1();
    }
    num_arguments = temp_arg;
}

/* The next set of token-handlers all have identical structure */
/* so we set up a macro BINARY_LOOP, with arguments:           */
/*   x=list of operator-tokens handled here,  y=next_handler   */

#define BINARY_LOOP(x,y) \
{\
    int op;\
    if (kn_status != SUCCESS) return;\
    y();\
    for(op = op_num(kn_token); is_in(op,(x));op = op_num(kn_token))\
    {\
        get_token();\
        y();\
        binary(op);\
    }\
}

static void logical1()
BINARY_LOOP(LOGICAL1,logical2)

static void logical2()
BINARY_LOOP(LOGICAL2,compares)

static void compares()
BINARY_LOOP(COMPARES,shifts)

static void shifts()
BINARY_LOOP(SHIFTS,arith1)

static void arith1()
BINARY_LOOP(ARITH1,arith2)

static void arith2()
BINARY_LOOP(ARITH2,exponents)

static void exponents()
{    
    if (kn_status != SUCCESS) return;
    unary_arith();
    if (is_in(op_num(kn_token),EXPONENT))
        while (is_in(op_num(kn_token),EXPONENT))
        {
            get_token();
            unary_arith();
            binary(EXP);
        }
}

static void unary_arith()  /* +(x) or -(x) */
{
    int op;

    if (kn_status != SUCCESS) return;
    op = op_num(kn_token);
    if (is_in(op,UNARYARITH))
    {
        get_token();
        transcend();
        if (op == SUB) unary(LCMP);
    }
    else
        transcend();
}

/* unary transcendental functions */
static void transcend()
{
    int op;

    if (kn_status != SUCCESS) return;
    op = op_num(kn_token);
    if (is_in(op,TRANSCEND))
        for ( ; is_in(op,TRANSCEND); op = op_num(kn_token))
        {
            get_token();
            prefixes();
            unary(op);
        }
    else
        prefixes();
}

static void prefixes() /* Functions of the form NAME(x,y...) */
{
    int op;
    
    if (kn_status != SUCCESS) return;
    op = op_num(kn_token);
    if (is_in(op,PREFIXES))
    {
        get_token();
        parentheses();
        multi(op);
    }
    else
        parentheses();
}

static void parentheses()
{
    if (kn_status != SUCCESS) return;
    if (*kn_token == '(')
    {
        get_token();
        commas();    /* Recursive ! */

        if(*kn_token != ')')
        {
            kn_status = BAD_FUNCTION_STRING;
            return;
        }
        get_token();
    }
    else
        primitive();
}


/**** End of Recursive Section ****/


/* load a number or variable onto the stack */
static void primitive()
{
    float val;
    if (kn_status != SUCCESS) return;
    switch(tok_type)
    {
        case VARIABLE:
            push_var(kn_var_index(kn_token));
            get_token();
            return;
        case NUMBER:
            val=atof(kn_token);
            push_static(val);
            get_token();
            return;
        default:
            kn_status = BAD_FUNCTION_STRING;
            return;
    }
}


/* multi: handles variable-argument operators */
static void multi(o)
int o;
{
    int i;

	check_arg_count(o,num_arguments);

    if (kn_status != SUCCESS) return;
    stack_to_register(o,num_arguments);
    
    for (i=num_arguments;i>0;i--) pop();
    push();

}

/* verifies that certain ops have the right # of args */
static void check_arg_count( op, num)
int op;
int num;
{
	if (is_in(op,PREFIX_ARG_COUNT) && num!=2)
		kn_status = BAD_FUNCTION_STRING;
}

/* binary: handles fixed 2-argument operators*/
static void binary(o)
int o;
{
    if (kn_status != SUCCESS) return;
    stack_to_register(o,2);
    
    pop();
    pop();
    push();
}

/* unary: handles single-arg operators */
static void unary(o)
int o;
{
    if (kn_status != SUCCESS) return;
    store_register(1);        /* store the register, if active */
    store_register(2);        /* store the register, if active */
    print_op(o,STK_PTR(1));    /* perform the operation */

    pop();
    push();
}


/* stack_to_register  scans through the first n stack    */
/* elements and records, in the code section of the      */
/* buffer, the necessary register commands to perform    */
/* the given operation upon the stack. If the operation  */
/* is commutative then we can optimize the code by       */
/* performing the operations in the most convenient order*/

static void stack_to_register(op,n_op)
int op;        /* input: the register op to perform */
int n_op;    /* input: the number of args to use  */
{
    int i,loaded;
    
    if (is_in(op,COMMUTATIVE))  /* commutative -- optimize ! */
    {
        for ((loaded=0,i=n_op); i>0; i--)
            if (load_stack(i)) loaded = i;
            
        /* perform the operations */
        if (loaded)
        {
            for (i=n_op;i>0;i--) 
                if (i != loaded) print_op(op,STK_PTR(i));
        }
        else for (i=n_op;i>0;i--)
            if (!ACTIVE_REGISTER(i)) print_op(op,STK_PTR(i));
    }
    else            /* operation is non-commutative */
    {
        store_register(1);    /* store register if active */
        load_stack(n_op);    /* load stack pointer if neccessary */
        for (i=n_op-1;i>0;i--)     /* perform the operations */
            print_op(op,STK_PTR(i));
    }
}

static void push() /* push active register onto the stack */
{
    push_var(top_of_register);
    kn_load = FALSE;     /* because a load would kill the active reg */
}


/* push constant onto the stack and store value in static */
static void push_static(value)
float value;
{
    if (top_of_static > MAXBUFFER)
    {
        kn_status = BAD_FUNCTION_STRING;
        return;
    }
    BUF_VAL(top_of_static) = value;
    push_var(top_of_static);
    top_of_static++;
}


static void push_var(ptr) /* push a pointer to buffer onto the stack */
int ptr;
{
    if (top_of_stack >= MAXSTACK || ptr<=0)
    {
        kn_status = BAD_FUNCTION_STRING;
        return;
    }
    kn_stack[top_of_stack]=ptr;
    top_of_stack++;

    /* check register to see if it hasn't been STORed yet */
    if (top_of_stack  >= 3)
        store_register(3);

    /* Check to see if LINE, etc are used */
	if (ptr==LINE_INDEX||ptr==SAMP_INDEX||ptr==BAND_INDEX)
		kn_uses_line_samp=TRUE;
}


static void pop() /* pop element from the stack */
{
    top_of_stack--;
    if (top_of_stack < BOTTOM_OF_STACK)
        kn_status = BAD_FUNCTION_STRING;
}

static void store_register(loc)
int loc;
{
    if (ACTIVE_REGISTER(loc))
    {
        print_op( STOR, STK_PTR(loc));
        BUF_VAL(loc) = 1.0;          /* flags register STORED */
        kn_load = TRUE;
        
        top_of_register+=2;          /* activate a new register */

        if (top_of_register > MAXBUFFER)
        {
            kn_status = BAD_FUNCTION_STRING;
            return;
        }
        BUF_VAL(top_of_register)=0.0; /* flags register ACTIVE */
    }
}

static int load_stack(loc)    /* load stack value if neccessary */
int loc;
{
    if ((kn_load) && !ACTIVE_REGISTER(loc) )
    {
        print_op(LOAD,STK_PTR(loc));
        kn_load = FALSE;
        return (TRUE);
    }
    
    return (FALSE);
}


static void print_op(op,operand)
int op;
int operand;
{    

    if (top_of_code > MAXBUFFER)
    {
        kn_status = BAD_FUNCTION_STRING;
        return;
    }
    OPCODE(top_of_code) = op;
    OPERAND(top_of_code) = operand;
    top_of_code+=2;
}

static int op_num(o) /* returns opcode index of a token */
char *o;
{
    int i;

    alt_char_set = 0;
    
    for(i=1;i<END_OP;i++)
        if (strncmp(o,op_name[i],strlen(op_name[i]))==0) return (i);
        
    for(i=1;i<END_OP;i++)
        if (strncmp(o,alt_op_name[i],strlen(alt_op_name[i]))==0)
        {
            alt_char_set = 1;  /* flags that code is in alt set */
            return (i);
        }
    for(i=1;i<END_OP;i++)
        if (strncmp(o,alt_op_name2[i],strlen(alt_op_name[i]))==0)
        {
            alt_char_set = 2;  /* flags that code is in alt set */
            return (i);
        }
    
    return (0);
}


static void get_token() /* gets the next token, or set kn_token to NULL at end */
{
    register char *temp;
    register char **c_set = 0;
    int op;
    
    tok_type = 0;
    temp = kn_token;
    
    while (iswhite(*kn_prog)) kn_prog++; /* skip over white space */
    
    op=op_num(kn_prog); /* search through list of operators */
    
    if (op)
    {
        tok_type = OPERATOR;
        
		switch(alt_char_set)
		{
			case 0:
			   c_set = op_name;
			   break;
			case 1:
			   c_set = alt_op_name;
			   break;
			case 2:
			   c_set = alt_op_name2;
			   break;
		}
        strcpy(kn_token,c_set[op]);
        kn_prog+=strlen(c_set[op]);
		
	/* Some ops are alphanumeric and should not have   */
	/* digits immediately following, eg. tan7 no good: */
		
	if (isalpha(kn_token[0]) && isdigit(*kn_prog))
		kn_status = BAD_FUNCTION_STRING;

        return;
    }
    else if (isalpha(*kn_prog))
    {
        tok_type = VARIABLE;
        while (!isdelim(*kn_prog)) *temp++ = *kn_prog++;
    }
    else if (isdigit(*kn_prog) || *kn_prog=='.')
    {
        tok_type = NUMBER;
        
        while (isdigit(*kn_prog)) *temp++ = *kn_prog++;   /* Get mantissa */
        if (( *kn_prog == '.') && !(op_num(kn_prog)))     /*   .decimal   */
        {
            *temp++ = *kn_prog++;
            while (isdigit(*kn_prog)) *temp++ = *kn_prog++;
        }

        if ( *kn_prog == 'E')                             /* Get exponent */
        {
            *temp++ = *kn_prog++;
            if (is_in(*kn_prog,"+-"))*temp++ = *kn_prog++;
            while (isdigit(*kn_prog)) *temp++ = *kn_prog++;
        }
    }

    *temp='\0';
}

/* check for  balanced parentheses, and convert to uppercase */
static void prepare_string()
{
    register int level;
    register char *cptr;
    
    for ( cptr=kn_prog, level=0; (*cptr); cptr++ )
        switch (*cptr)
        {
            case '(':
                level ++;
                break;
            case ')':
                level --;
                break;
            case '$': /* For Backwards compatibility */
                *cptr = '\0';
                if (level != 0)
                      kn_status = BAD_FUNCTION_STRING;
                return;
            default:
                *cptr = toupper(*cptr);
                break;
        }
    
       if (level != 0)
            kn_status = BAD_FUNCTION_STRING;
}


/* string processing */

static int iswhite(c)
char c;
{
    /* look for spaces and tabs */
    if (c==' ' || c==9) return (TRUE);
    return (FALSE);
}

static int isdelim(c)
char c;
{
    if (is_in(c,DELIMSTRING) || c==9 || c==' ' || c=='\r' || c==0)
        return (TRUE);
    return (FALSE);
}

static int is_in(ch,s)
char ch,*s;
{
    while (*s) if (*s++ == ch) return (TRUE);
    return (FALSE);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create knuth_var.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* KNUTH_VAR: returns index of a knuth-supported variable name */

#include "knuth_int.h"

int kn_var_index(char* cname);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2_(knuth_var, KNUTH_VAR) (char *fname, int *value, ZFORSTR_PARAM)
#if 0
char *fname;        /* input: knuth variable name         */
int *value;         /* output: index of knuth variable   */
#endif
{
   ZFORSTR_BLOCK
   char cname[50];
   int length,index;

   zsfor2len(length, fname, &fname, 2, 1, 1, value);
   zsfor2c(cname, length, fname, &fname, 2, 1, 1, value);
   index=kn_var_index(cname);
   *value= index ? index : (-1) ;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

/* This returns the index of the given token name */
/* Decremented by one for C-arrays                */

int zknuth_var(cname)
char *cname;
{
	return (kn_var_index(cname) - 1);
}

/************************************************************************/
/* The common subroutine                                                */
/************************************************************************/
int kn_var_index(cname)
char *cname;
{
    switch (toupper(*cname))
    {
        case 'I':        /* IN1, IN2, ... IN18 */
            if (isdigit(*(cname+2))) return (atoi(cname+2));
            break;
        case 'D':        /* DN = IN1 */
            return (1);
        case 'L':        /* LINE */
            return (LINE_INDEX);
        case 'S':        /* SAMP */
            return (SAMP_INDEX);
        case 'B':        /* BAND */
            return (BAND_INDEX);
        default:         /* X1, C1, etc = IN1 */
            if (isdigit(*(cname+1))) return (atoi(cname+1));
            break;
    }
    return (0);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create knuth_dump.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* KNUTH_DUMP: Ascii symbolic dump of compiled code */

#include "knuth_int.h"
#include <stdio.h>
#include "zvproto.h"

/* OpCode Names: Should be kept in parallel with knuth, xknuth */
static char *op_code_name[]= {          
      "x","ADD","SUB","MUL","DIV","EXP","LOG","LN","INT","x",
      "x","x","x","LOAD","STOR","RETN","SQRT","SIN","COS","TAN",
      "MAX","MIN","MOD","ABS","LCMP","ATN2","ASIN","ACOS","ATAN","LT",
      "LE","EQ","NE","GE","GT","OR","AND","XOR","NOT",
      "LAND","LOR","LSHF","RSHF"};

#define KNBUFFER buf


/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2_(knuth_dump, KNUTH_DUMP) (operator *buf)
#if 0
operator *buf;        /* input: compiled code and registers     */
#endif
{
   zknuth_dump((float *)buf);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zknuth_dump(
  float *buff        /* input: compiled code and registers     */
)
{
   operator *buf = (operator *)buff;
   int done;
   operator *op,*top;
   char op_msg[20];

    
    top = OPBUFFER( MAXBUFFER );
	done=0;
    for (op=OPBUFFER(BOTTOM_OF_CODE); op<top && !done; op+=2)
	{
		sprintf(op_msg,"   %-4s %3d",op_code_name[op->opcode],op->operand);
		zvmessage(op_msg,"");
		done=(op->opcode == RTN);
	}
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create knuth_lookup.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*                              

  subroutine knuth_lookup (Alias xkn65_exe):

  This subroutine generates a 256x256 table for IN1 and IN2 in order
  to calculate every possible outcome for the two input byte data.  If only
  one input IN1 is used, then a 256 array is generated. Schematically,

            table[ IN2 ][ IN1 ] = xknuth(IN1,IN2)   (num_input=2);

            table[ IN1 ] = xknuth(IN1)              (num_input=1)
            
  NOTE: in FORTRAN the BYTE data type is a signed data type, even though
  it is often interpreted as unsigned. For this reason, a flag may be
  passed to indicate that BYTE indexing will be used. In this case, the
  table should be declared and used as:
  
              BYTE TABLE_NAME(-128:127,-128:127)
              BYTE TABLE2(-128:127)
              BYTE IN1,IN2,VAL
            
            VAL=TABLE_NAME(IN1,IN2)   (num_input=2)
            
            VAL=TABLE2(IN1)           (num_input=1)
*/

#include "knuth_int.h"
#define KNBUFFER buf

/************************************************************************/
/* Fortran-Callable Version(s)                                          */
/************************************************************************/

/* old version for backward compatibility */

void FTN_NAME2_(xkn65_exe, XKN65_EXE)
(
  float *buf,          /* input:  variables,constants,operators,operands  */
  unsigned char *table,/* output: the 256x256 combinations of IN1 and IN2 */
  int *count,          /* output: the number of iterations performed      */
  int *num_input,      /* input:  the number of input files               */
  int *flags          /* input:  numbers are rounded or truncated ?      */
)
{
    zknuth_lookup(buf,table,*num_input,*flags);
    *count = (*num_input == 1) ? 1 : 256;
}

#define KN_ROUND 1
#define KN_BYTE_INDEX 2

/* new name so humans have a clue what this thing does */
/* count parameter is omitted in this new version.     */

void FTN_NAME2_(knuth_lookup, KNUTH_LOOKUP)
(
  float *buf,          /* input:  variables,constants,operators,operands  */
  unsigned char *table,/* output: the 256x256 combinations of IN1 and IN2 */
  int *num_input,      /* input:  the number of input files               */
  int *flags           /* input:  numbers are rounded or truncated ?      */
)
{
    zknuth_lookup(buf,table,*num_input,*flags);
}

	 

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zknuth_lookup(buf,table,num_input,flags)
float *buf;          /* input:  variables,constants,operators,operands  */
unsigned char *table;/* output: the 256x256 combinations of IN1 and IN2 */
int num_input;       /* input:  the number of input files               */
int flags;           /* bit1:   numbers are rounded(1) or truncated(0)? */
                     /* bit2:   buffer index uses fortran BYTE indexing */
{
     register int i,j,iresult;
     int index_val[256]; 
     int round;
     float result;
      
      round=flags & KN_ROUND;
      if (flags & KN_BYTE_INDEX)
	       for (i=0;i<256;i++) index_val[i]= (i+128) % 256;
      else
	       for (i=0;i<256;i++) index_val[i]= i;

      if (num_input == 1)
      {
         for (i=0;i<256;i++)
         {
             BUF_VAL(1) = index_val[i];
               zxknuth(buf,&result);
               iresult = round ? ROUND(result) : TRUNCATE(result);
               *table = BYTE_RANGE(iresult);
               table++;
         }
      }
      else
      {
           for (j=0;j<256;j++)
         {
               BUF_VAL(2) = index_val[j];
               for (i=0;i<256;i++)
               {
                     BUF_VAL(1) = index_val[i];
                     zxknuth(buf,&result);
                     iresult = round ? ROUND(result) : TRUNCATE(result);
                     *table = BYTE_RANGE(iresult);
                     table++;
               }
          }
      }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xknuth.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "knuth_int.h"
#include <zvproto.h>
#define KNBUFFER buf

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(xknuth, XKNUTH) (float *buf,float *result)
#if 0
float *buf;        /* input: compiled code and registers     */
float *result;     /* output: resultant of computation       */
#endif
{
   zxknuth(buf,result);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zxknuth(buf,result)
float *buf;        /* input: compiled code and registers     */
float *result;     /* output: resultant of computation       */
{
    register float reg = 0.0,temp;
    register operator *op,*top;
    
    top = OPBUFFER( MAXBUFFER );

    for (op=OPBUFFER(BOTTOM_OF_CODE); op<top; op+=2)
    switch(op->opcode)
    {
        case LOAD:
            reg = BUF_VAL(op->operand);
            break;
        case STOR:
            BUF_VAL(op->operand) = reg;
            break;
        case LCMP:
            reg = -BUF_VAL(op->operand);
            break;
        case NOT:
            reg = !((int)(BUF_VAL(op->operand)));
            break;
        case SUB:
            reg-= BUF_VAL(op->operand);
            break;
        case ADD:
            reg+= BUF_VAL(op->operand);
            break;
        case MUL:
            reg*= BUF_VAL(op->operand);
            break;
        case DIV:
            temp= BUF_VAL(op->operand);
            reg/= NONZERO(temp);
            break;
        case SIN:
            reg = sin(BUF_VAL(op->operand));
            break;
        case COS:
            reg = cos(BUF_VAL(op->operand));
            break;
        case TAN:
            reg = tan(BUF_VAL(op->operand));
            break;
        case ASIN:
            temp = BUF_VAL(op->operand);
            reg = asin(GOODTRIG(temp));
            break;
        case ABS:
            reg = fabs(BUF_VAL(op->operand));
            break;
        case ACOS:
            temp = BUF_VAL(op->operand);
            reg = acos(GOODTRIG(temp));
            break;
        case ATAN:
            reg = atan(BUF_VAL(op->operand));
            break;
        case LN:
            temp = fabs(BUF_VAL(op->operand));
            reg = log(ABOVEZERO(temp));
            break;
        case LOG10:
            temp = fabs(BUF_VAL(op->operand));
            reg = log10(ABOVEZERO(temp));
            break;
        case SQRT:
            reg = sqrt(fabs(BUF_VAL(op->operand)));
            break;
        case INT:
            temp = BUF_VAL(op->operand);
            reg = (int)(LEGALINT(temp));
            break;
        case RTN:
            *result=reg;
            return ;
        case LT:
            reg = (reg < BUF_VAL(op->operand));
            break;
        case LE:
            reg = (reg <= BUF_VAL(op->operand));
            break;
        case EQ:
            reg = (reg == BUF_VAL(op->operand));
            break;
        case NE:
            reg = (reg != BUF_VAL(op->operand));
            break;
        case GE:
            reg = (reg >= BUF_VAL(op->operand));
            break;
        case GT:
            reg = (reg > BUF_VAL(op->operand));
            break;
        case OR:
            reg = ((int)reg | (int)BUF_VAL(op->operand));
            break;
        case LOR:
            reg = ((int)reg || (int)BUF_VAL(op->operand));
            break;
        case AND:
            reg = ((int)reg & (int)BUF_VAL(op->operand));
            break;
        case RSHF:
            reg = ((int)reg >> (int)BUF_VAL(op->operand));
            break;
        case LSHF:
            reg = ((int)reg << (int)BUF_VAL(op->operand));
            break;
        case LAND:
            reg = ((int)reg && (int)BUF_VAL(op->operand));
            break;
        case XOR:
            reg = ((int)reg ^ (int)BUF_VAL(op->operand));
            break;
        case ATAN2:
            temp = BUF_VAL(op->operand);
            if (temp == 0.0 && reg == 0.0)
             reg = 0.0;
            else 
              reg = atan2(reg,temp);
            break;
        case MOD:
            temp = BUF_VAL(op->operand);
            reg = fmod(reg,NONZERO(temp));
            break;
        case AMAX:
            temp = BUF_VAL(op->operand);
            reg = MAX(reg,temp);
            break;
        case AMIN:
            temp = BUF_VAL(op->operand);
            reg = MIN(reg,temp);
            break;
        case EXP:
            temp= fabs(reg);
            reg = pow( ABOVEZERO(temp), BUF_VAL(op->operand) );
            break;
        default:
            zvmessage("UNIMPLEMENTED OPCODE","");
            return ;
            break;
    }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xknuth_comp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "knuth_int.h"
#include <zvproto.h>

/*
 *  xknuth_complex -- provides a complex-valued implementation
 *    of the compiled knuth array. This requires that all of the
 *    input variables (including LINE,SAMP and BAND)
 *    be standard complex numbers: declared COMPLEX in FORTRAN, or
 *    as "float" (real,imaginary) pairs. The "result" must also
 *    be complex.
 */

#define KNBUFFER (buf)
#define COMPLEX_MAG2(z) (z.c_real*z.c_real + z.c_imag*z.c_imag)
#define COMPLEX_MAG(z) (sqrt(COMPLEX_MAG2(z)))
#define COMPLEX_ARG(z) (atan2(z.c_imag,z.c_real))

static void realign_buffer_for_complex();

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2_(knuth_complex, KNUTH_COMPLEX) (char *string, complex_type *buf,
		int *status, ZFORSTR_PARAM)
#if 0
char *string;        /* input: string to parse                */
complex_type *buf;   /* output: compiled code & registers     */
int *status;         /* output: error code                    */
#endif
{
   ZFORSTR_BLOCK
   char *c_string;
   int length;

   zsfor2len(length, string, &string, 3, 1, 1, status);
   c_string = (char *)calloc(1,(length+1));
   zsfor2c(c_string, length, string, &string, 3, 1, 1, status);

   /* call C routine */
   *status = zknuth_complex(c_string,buf);

   free (c_string);
}


void FTN_NAME2_(xknuth_complex, XKNUTH_COMPLEX)
(
  complex_type  *buf,        /* input: compiled code and registers     */
  complex_type  *result      /* output: resultant of computation       */
)
{
   zxknuth_complex(buf,result);
}


/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

int zknuth_complex(char* string, complex_type* buf)
{
  int status=zknuth(string,(float*) buf);

	if (status>1) return status;
	
	realign_buffer_for_complex(buf);
	
    return status;
}

#define NEW_OPBUFFER(x) (OPBUFFER(2*(x-1)+1))

void zxknuth_complex(complex_type  *buf,complex_type  *result)
{
    register operator *op,*top;
    register complex_type reg,temp,temp2;
	double mag;
	double ztan,ztanh;
    reg.c_imag = 0.0;
    reg.c_real = 0.0;
	
    top = NEW_OPBUFFER( MAXBUFFER );

    for (op=NEW_OPBUFFER(BOTTOM_OF_CODE); op<top; op+=4)
    switch(op->opcode)
    {
        case LOAD:
            reg = BUF_VAL(op->operand);
            break;
        case STOR:
            BUF_VAL(op->operand) = reg;
            break;
        case LCMP:
            reg = (BUF_VAL(op->operand));
            reg.c_real = -reg.c_real;
            reg.c_imag = -reg.c_imag;
            break;
        case NOT:
            reg.c_real = !((int)(BUF_VAL(op->operand).c_real));
            reg.c_imag = 0.0;
            break;
        case SUB:
            temp = BUF_VAL(op->operand);
            reg.c_real-= temp.c_real;
            reg.c_imag-= temp.c_imag;
            break;
        case ADD:
            temp = BUF_VAL(op->operand);
            reg.c_real+= temp.c_real;
            reg.c_imag+= temp.c_imag;
            break;
        case MUL:
            temp = BUF_VAL(op->operand);
            temp2 = reg;
            reg.c_real = temp.c_real*temp2.c_real - temp.c_imag*temp2.c_imag;
            reg.c_imag = temp.c_real*temp2.c_imag + temp.c_imag*temp2.c_real;
            break;
        case DIV:
            temp = BUF_VAL(op->operand);
            temp2 = reg;
			mag = COMPLEX_MAG2(temp);
			mag =  ABOVEZERO(mag);
            reg.c_real = (temp.c_real*temp2.c_real + temp.c_imag*temp2.c_imag)/mag;
            reg.c_imag = (temp.c_real*temp2.c_imag - temp.c_imag*temp2.c_real)/mag;
            break;
        case ABS:
            temp = BUF_VAL(op->operand);
            reg.c_real = COMPLEX_MAG(temp);
            reg.c_imag = 0.0;
            break;
         case SIN:
            temp = BUF_VAL(op->operand);
            reg.c_real = cosh(temp.c_imag)*sin(temp.c_real);
            reg.c_imag = -sinh(temp.c_imag)*cos(temp.c_real);
            break;
        case COS:
            temp = BUF_VAL(op->operand);
            reg.c_real = cosh(temp.c_imag)*cos(temp.c_real);
            reg.c_imag = sinh(temp.c_imag)*sin(temp.c_real);
            break;
        case TAN:
            temp = BUF_VAL(op->operand);
			ztan = tan(temp.c_real);
			ztanh = tanh(temp.c_imag);
			mag = 1.0 + ztan*ztan*ztanh*ztanh;
			mag =  ABOVEZERO(mag);			
            reg.c_real = ztan*(ztanh*ztanh + 1.0)/mag;
            reg.c_imag = ztanh*(ztan*ztan - 1.0)/mag;
            break;
        case LN:
            temp = BUF_VAL(op->operand);
			mag = COMPLEX_MAG(temp);
            reg.c_real = log(ABOVEZERO(mag));
            reg.c_imag = COMPLEX_ARG(temp);
            break;
        case LOG10:
            temp = BUF_VAL(op->operand);
			mag = COMPLEX_MAG(temp);
            reg.c_real = log10(ABOVEZERO(mag));
            reg.c_imag = COMPLEX_ARG(temp)/log(10);
            break;
        case SQRT:
            temp = BUF_VAL(op->operand);
			mag = COMPLEX_MAG(temp);
            reg.c_real = sqrt((temp.c_real + mag)/2);
            reg.c_imag = sqrt((mag - temp.c_real)/2);
            break;
            break;
        case INT:
            temp = BUF_VAL(op->operand);
            reg.c_real = (int)(temp.c_real);
            reg.c_imag = (int)(temp.c_imag);
            break;
        case RTN:
            *result=reg;
            return ;
        case LT:
            reg.c_real = (reg.c_real < BUF_VAL(op->operand).c_real);
            reg.c_imag = 0.0;
            break;
        case LE:
            reg.c_real = (reg.c_real <= BUF_VAL(op->operand).c_real);
            reg.c_imag = 0.0;
            break;
        case EQ:
            temp = BUF_VAL(op->operand);
            reg.c_real = (reg.c_real == temp.c_real
				&& reg.c_imag == temp.c_imag);
    	    reg.c_imag = 0.0;
            break;
        case NE:
            temp = BUF_VAL(op->operand);
            reg.c_real = !(reg.c_real == temp.c_real
			&& reg.c_imag == temp.c_imag);
	    reg.c_imag = 0.0;
            break;
        case GE:
            reg.c_real = (reg.c_real >= BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case GT:
            reg.c_real = (reg.c_real > BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case OR:
            reg.c_real = ((int)reg.c_real | (int)BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case LOR:
            reg.c_real = ((int)reg.c_real || (int)BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case AND:
            reg.c_real = ((int)reg.c_real & (int)BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case RSHF:
            reg.c_real = ((int)reg.c_real >> (int)BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case LSHF:
            reg.c_real = ((int)reg.c_real << (int)BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case LAND:
            reg.c_real = ((int)reg.c_real && (int)BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case XOR:
            reg.c_real = ((int)reg.c_real ^ (int)BUF_VAL(op->operand).c_real);
		reg.c_imag = 0.0;
            break;
        case MOD:
            temp = BUF_VAL(op->operand);
            reg.c_real = fmod(reg.c_real,NONZERO(temp.c_real));
            reg.c_imag = fmod(reg.c_imag,NONZERO(temp.c_imag));
            break;
        case AMAX:
            temp = BUF_VAL(op->operand);
            reg.c_real = MAX(reg.c_real,temp.c_real);
            reg.c_imag = MAX(reg.c_imag,temp.c_imag);
            break;
        case AMIN:
            temp = BUF_VAL(op->operand);
            reg.c_real = MIN(reg.c_real,temp.c_real);
            reg.c_imag = MIN(reg.c_imag,temp.c_imag);
            break;
        case EXP:

            temp = BUF_VAL(op->operand);
			temp2 = reg;
			
			/* natural log temp2 */
			mag = COMPLEX_MAG(temp2);
            reg.c_real = log(ABOVEZERO(mag));
            reg.c_imag = COMPLEX_ARG(temp2);

			/* multiply temp1 */
            temp2.c_real = temp.c_real*reg.c_real - temp.c_imag*reg.c_imag;
            temp2.c_imag = temp.c_real*reg.c_imag + temp.c_imag*reg.c_real;

			/* exponentiate product */
			temp2.c_real = exp(temp2.c_real);
			reg.c_real = temp2.c_real * cos( temp2.c_imag );
			reg.c_imag = temp2.c_real * sin( temp2.c_imag );

            break;
        case ASIN:
        case ACOS:
        case ATAN:
        case ATAN2:
          zvmessage("UNIMPLEMENTED COMPLEX OPCODE","");
            return ;
            break;
 
 		default:
            zvmessage("UNIMPLEMENTED OPCODE","");
            return ;
            break;
    }
}

#define REALBUF_VAL(x)         (*((float *)KNBUFFER + (x) - 1))

/*
 * realign_buffer_for_complex:
 *
 * sets code and values up for proper complex addressing.
 * The problem is that the compiled code is 4-byte, while
 * everything else is 8. Plus, the REGISTER locations alternate
 * with the CODE addresses! The memory mapping is:
 *
 *    VARIABLES  8-BYTE
 *    STATIC     8-BYTE
 *    CODE       4-BYTE / REGISTERS  8-BYTE (alternates with CODE)
 *    
 *
 * To make this as simple as possible, we just start at the top
 * of the code and work down, moving each code and data element to 
 * the ("realpart") locations. 
 */
static void realign_buffer_for_complex(buf)
complex_type  *buf;        /* input: compiled code and registers     */
{
	int i;

	/* Move CODE segment up */
	for (i=BOTTOM_OF_CODE+MAXCODE-1;i>=BOTTOM_OF_CODE;i--)
		*NEW_OPBUFFER(i) = *OPBUFFER(i);
	
	/* Move DATA segment up */
	for (i=BOTTOM_OF_STATIC+MAXSTATIC/2-1;i>=BOTTOM_OF_STATIC;i--)
	{
		BUF_VAL(i).c_real = REALBUF_VAL(i);
		BUF_VAL(i).c_imag = 0.0;
	}
}




$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create knuth_int.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* knuth_int.h -- Internal Header file for knuth */

/* NOTE: This file is used for both modules KNUTH, XKNUTH,
 * NOTE  and XKNUTH_COMPLEX. If you need to make changes here 
 * NOTE: for one module, please modify the other one as well.
 */


#ifndef _H_KNUTH_INT
#define _H_KNUTH_INT

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>    /* needed for atof */
#include <ctype.h>
#include <math.h>
#include "knuth.h"	/* external prototypes */

/* fundamental data structure used in knuth */
typedef struct 
{
    short operand;
    short opcode;
} operator;


/* token types */
#define OPERATOR 1
#define VARIABLE 2
#define NUMBER 3

/* array sizes */
#define MAXSTRLEN 300
#define MAXSTACK 100

#define MAXVARIABLES 51
#define MAXSTATIC 40
#define MAXCODE 100
#define MAXREGISTER MAXCODE
#define MAXBUFFER MAXVARIABLES+MAXSTATIC+MAXCODE+MAXREGISTER


/* locations of storage areas */
#define BOTTOM_OF_STACK 0

#define LINE_INDEX          MAXVARIABLES-2
#define SAMP_INDEX          LINE_INDEX+1
#define BAND_INDEX          SAMP_INDEX+1
#define BOTTOM_OF_STATIC    1+MAXVARIABLES
#define BOTTOM_OF_CODE      BOTTOM_OF_STATIC+MAXSTATIC
#define BOTTOM_OF_REGISTER  BOTTOM_OF_CODE+1

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

/* status codes */
#define SUCCESS              0  /* success */
#define USES_LINE_SAMP       1  /* Warning */
#define BAD_FUNCTION_STRING  2  /* Error   */
#define EVALUATION_ERROR     3  /* Error   */

/* Useful Macros for accessing buffer and values */
/* KNBUFFER is defined locally in knuth, xknuth/complex  */

#define BUF_VAL(x)         (*(KNBUFFER + (x) - 1))
#define OPBUFFER(x)        ((operator *)KNBUFFER + (x) - 1)
#define OPCODE(x)          OPBUFFER(x)->opcode
#define OPERAND(x)         OPBUFFER(x)->operand

/* For avoiding divide-by-zero, etc. on evaluations: */
#ifndef MAX
#define MAX(x,y)     (((x)>(y)) ? (x) : (y))
#define MIN(x,y)     (((x)<(y)) ? (x) : (y))
#endif
#define SMALL 1e-20
#define ABOVEZERO(x) MAX((x),SMALL)
#define BELOWZERO(x) ( ((x) < -SMALL) ? (x) : -SMALL )
#define NONZERO(x)   ( ((x) >= 0) ? ABOVEZERO(x) : BELOWZERO(x) )
#define KN_MAXINT 2147483647.0
#define KN_MININT -2147483648.0
#define LEGALINT(x) MAX( MIN( (x), KN_MAXINT ), KN_MININT )
#define GOODTRIG(x)   ( ((x)>1) ? 1 : ( ((x)<-1) ? -1 : (x) ) )
#define BYTE_RANGE(x) MAX(0,MIN(x,255))
#define ROUND(x)    (((x)>=0) ? (int)(floor((x)+0.5)):(int)(ceil((x)-0.5)) )
#define TRUNCATE(x) (((x)>=0) ? (int)floor(x) : (int)ceil(x))


/* operator codes. VALUE 12 IS OMITTED FOR HISTORICAL REASONS*/

#define ADD        1
#define SUB        2
#define MUL        3
#define DIV        4
#define EXP        5
#define LOG10      6
#define LN         7
#define INT        8
#define LPAREN     9
#define COMMA      10
#define RPAREN     11
#define LOAD       13
#define STOR       14
#define RTN        15
#define SQRT       16
#define SIN        17
#define COS        18
#define TAN        19
#define AMAX       20
#define AMIN       21
#define MOD        22
#define ABS        23
#define LCMP       24
#define ATAN2      25
#define ASIN       26
#define ACOS       27
#define ATAN       28
#define LT         29
#define LE         30
#define EQ         31
#define NE         32
#define GE         33
#define GT         34
#define OR         35
#define AND        36
#define XOR        37
#define NOT        38
#define LAND       39
#define LOR        40
#define LSHF       41
#define RSHF       42
#define END_OP     43

/* end of Header file knuth_int.h */

#endif /* _H_KNUTH_INT */

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create knuth.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY knuth

   To Create the build file give the command:

    $ vimake knuth                     (VMS)
   or
    % vimake knuth                     (Unix)


*************************************************************************/

#define SUBROUTINE knuth

#define MODULE_LIST knuth.c knuth_var.c knuth_dump.c \
	knuth_lookup.c xknuth.c xknuth_comp.c
#define INCLUDE_LIST knuth_int.h

#define FTN_STRING
#define P1_SUBLIB

#define USES_ANSI_C

/*#define LIB_LOCAL	/* remove on delivery */
/*#define DEBUG		/* remove on delivery */
/*************************************************************************/
$ Return
$!#############################################################################
$Test_File:
$ create tknuth.f
C  2-87  SP   TEST PROGRAM WAS NOT WORKING IN SOME CASES BECAUSE IT EMBEDDED
C             3 SPACES BETWEEN EVERY CHARACTER OF FORMULA BEFORE CALLING KNUTH.
C             (BOMBED ON .OR.,.AND. ...)  MODIFIED TO NOT ADD BLANKS.
C  9-90  JFM  TEST EXPANDED TO TEST MORE BAD FUNCTION STRINGS
C  2-92  NDR  TEST EXPANDED TO TEST NEW OPTIONS & MORE BAD FUNCTION STRINGS;
C               "IN1.EQ.-4" SHOULD YIELD VALID RESULT: '-' HAS HIGHER PRIORITY.
C               ELIMINATED DEPENDENCE ON NON-UNIX BINBCD,PRNT.
C  4-92  NDR   TEST EXPANDED TO TEST XKNUTH AND KNUTH_LOOKUP, TRUNCATE & ROUND;
C                ELIMINATED NON-PORTABLE EQUIVALENCES
C 11-94  NDR   TEST EXPANDED TO TEST XKNUTH_COMP COMPLEX VALUED OPS
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INCLUDE 'fortport'
      BYTE TABLE(0:255,0:255)
      BYTE BTABLE(-128:127,-128:127)
      EQUIVALENCE(TABLE,BTABLE)
      INTEGER I,J
      BYTE IB,JB
      CHARACTER*250 FCN
      CHARACTER*80 OUTMSG
      INTEGER*2 IBUF(2,150)
      INTEGER COUNT, IER,INDEX
      REAL FBUF(300),RESULT
      EQUIVALENCE(FBUF,IBUF)
      LOGICAL XVPTST


C --- Get the function
      CALL XVP ('FUNCTION', FCN, COUNT)

C--- Test the VARIABLE INDEX subroutine
      IF (XVPTST('KNVAR')) THEN
          CALL KNUTH_VAR(FCN,INDEX)
          WRITE(UNIT=OUTMSG,FMT='(A,A5,A,I3)')
     +           'VARIABLE "',FCN,'" INDEX=',INDEX
          CALL XVMESSAGE(OUTMSG,' ')
      ENDIF

C --- Compile the function
      CALL KNUTH (FCN,IBUF,IER)
      IF (IER.EQ.2) GO TO 400

C ---    Show compiled function
      CALL KNUTH_DUMP(IBUF)

      IF (XVPTST('XKNUTH')) THEN
	  	IF (XVPTST('COMPLEX')) THEN
			CALL TEST_COMPLEX(FCN)
			RETURN
		 ENDIF
C ---   Execute the function on a few REAL values
          CALL XVMESSAGE('FUNCTION VALUES:',' ')
          DO I = 0,3,3
          DO J = 0,2,2
          FBUF(1) = FLOAT(I)
          FBUF(2) = FLOAT(J)
          CALL XKNUTH(FBUF,RESULT)
          WRITE(UNIT=OUTMSG,FMT='(4X,A,F6.1,A,F6.1,A,F6.1)')
     +           'XKNUTH(',FBUF(1),',',FBUF(2),')=',RESULT
          CALL XVMESSAGE(OUTMSG,' ')
          ENDDO
          ENDDO
C --- Added test for values > 127:
		  FBUF(1)=130.
		  FBUF(2)=255.
          CALL XKNUTH(FBUF,RESULT)
          WRITE(UNIT=OUTMSG,FMT='(4X,A,F6.1,A,F6.1,A,F6.1)')
     +           'XKNUTH(',FBUF(1),',',FBUF(2),')=',RESULT
          CALL XVMESSAGE(OUTMSG,' ')
    
C ---   Use the Byte lookup table method - two variables & truncate
          CALL KNUTH_LOOKUP(IBUF,TABLE,2,0)
          CALL XVMESSAGE('LOOKUP TABLE VALUES (trunc.,int indx):',' ')
          DO I = 0,3,3
          DO J = 0,2,2
                  WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +            'TABLE(',I,',',J,')=',BYTE2INT( TABLE(I,J) )
                  CALL XVMESSAGE(OUTMSG,' ')
          ENDDO
          ENDDO
C --- Added test for values > 127:
          CALL KNUTH_LOOKUP(IBUF,BTABLE,2,2)
          CALL XVMESSAGE('LOOKUP TABLE VALUE (trunc,byte indx):',' ')
		  IB=-126   ! 130 unsigned
		  JB=-1     ! 255 unsigned
          WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +          'TABLE(',130,',',255,')=',
     +          BYTE2INT(BTABLE(IB,JB))
          CALL XVMESSAGE(OUTMSG,' ')
    
C ---   Use the Byte lookup table method - two variables & round
          CALL KNUTH_LOOKUP(IBUF,TABLE,2,1)
          CALL XVMESSAGE('LOOKUP TABLE VALUES (round):',' ')
          DO I = 0,3,3
          DO J = 0,2,2
                  WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +           'TABLE(',I,',',J,')=',BYTE2INT( TABLE(I,J) )
                  CALL XVMESSAGE(OUTMSG,' ')
          ENDDO
          ENDDO
C --- Added test for values > 127:
          CALL KNUTH_LOOKUP(IBUF,BTABLE,2,3)
          CALL XVMESSAGE('LOOKUP TABLE VALUE (round,byte index):',' ')
		  IB=-126   ! 130 unsigned
		  JB=-1     ! 255 unsigned
          WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +          'TABLE(',130,',',255,')=',
     +          BYTE2INT(BTABLE(IB,JB))
          CALL XVMESSAGE(OUTMSG,' ')
      ENDIF
      RETURN
400   CALL XVMESSAGE('BAD FUNCTION STRING',' ')
      CALL ABEND
      RETURN
      END

      SUBROUTINE TEST_COMPLEX(FCN)
	  IMPLICIT NONE
	  CHARACTER*(*) FCN
	  COMPLEX*8 CBUF(300),CRESULT
	  REAL*4 FBUF(300),FRESULT
	  CHARACTER*80 OUTMSG
	  INTEGER COUNT,status
	  EQUIVALENCE(CBUF,FBUF)
	  

	  CALL KNUTH_COMPLEX(FCN,CBUF,status)	  
	  CALL XVP('CVALUES',FBUF,COUNT)
	  CALL XKNUTH_COMPLEX(CBUF,CRESULT)	  
	  WRITE (OUTMSG,'(A,F7.3,F7.3)') 'COMPLEX-RESULT=',CRESULT
	  CALL XVMESSAGE(OUTMSG,' ')
	  
	  CALL KNUTH(FCN,FBUF,status)	  
	  CALL XVP('CVALUES',FBUF,COUNT)
	  CALL XKNUTH(FBUF,FRESULT)	  
	  WRITE (OUTMSG,'(A,F7.3)') 'REAL-RESULT=',FRESULT
          CALL XVMESSAGE(OUTMSG,' ')

	  RETURN
	  END


$!-----------------------------------------------------------------------------
$ create tknuth.imake
/***********************************************************************

                     IMAKE FILE FOR TEST PROGRAM tknuth

   To Create the build file give the command:

    $ vimake tknuth                     (VMS)
   or
    % vimake tknuth                     (Unix)

*************************************************************************/

#define PROGRAM tknuth

#define MODULE_LIST tknuth.f

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define FTNINC_LIST fortport

#define USES_FORTRAN
#define MAIN_LANG_FORTRAN

/*#define LIB_LOCAL	/* remove on delivery */
$!-----------------------------------------------------------------------------
$ create tknuth.pdf
PROCESS
PARM FUNCTION TYPE=(STRING,130) DEFAULT="IN1"
PARM MODULE KEYWORD VALID=(KNUTH,XKNUTH,KNVAR) DEF=KNUTH
PARM FORMAT KEYWORD VALID=(COMPLEX,REAL) DEF=REAL
PARM CVALUES REAL COUNT=0:20 DEF=(0,0)
END-PROC
$!-----------------------------------------------------------------------------
$ create tstknuth.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
write "*** Test KNUTH, KNUTH_DUMP ***"
TKNUTH
TKNUTH  FUNCTION="LINE"
TKNUTH  FUNCTION="SAMP"
TKNUTH  FUNCTION="IN1+1"
TKNUTH  FUNCTION="SQRT(IN1**1.1234+7)"
TKNUTH  FUNCTION="SQRT(IN1**1.1234e1+7)"
TKNUTH  FUNCTION="IN1.AND.IN2"
TKNUTH  FUNCTION="IN1.OR.IN2"
TKNUTH  FUNCTION="IN1.XOR.IN2"
TKNUTH  FUNCTION="IN1.LT.IN2"
TKNUTH  FUNCTION="IN1.GT.IN2"
TKNUTH  FUNCTION="IN1.LE.IN2"
TKNUTH  FUNCTION="IN1.EQ.IN2"
TKNUTH  FUNCTION="IN1.EQ..5"
TKNUTH  FUNCTION="5..EQ.IN1"
TKNUTH  FUNCTION=".NOT.(IN1.GE.IN2)"
TKNUTH  FUNCTION="MOD(IN2,IN1)"
TKNUTH  FUNCTION="MIN(IN1,IN2)"
TKNUTH  FUNCTION="MAX(IN1,IN2)"
TKNUTH  FUNCTION="MAX(COS(SIN(-IN1)+-3)/2,TAN(0))"
TKNUTH  FUNCTION="MAX(IN1,IN2,IN3,IN4,IN5)"
TKNUTH  FUNCTION="100.*SIN(IN1/10.)+100.*COS(IN2/10)"
TKNUTH  FUNCTION="LINE+SAMP"
TKNUTH  FUNCTION="MOD(SAMP,LINE)"
write "*** Test C-language Constructs ***"
TKNUTH  FUNCTION="IN1 | IN2"
TKNUTH  FUNCTION="IN1 & IN2"
TKNUTH  FUNCTION="IN1 % IN2"
TKNUTH  FUNCTION="IN1 ^ IN2"
TKNUTH  FUNCTION="!IN1"
TKNUTH  FUNCTION="IN1 < IN2"
TKNUTH  FUNCTION="IN1 > IN2"
TKNUTH  FUNCTION="IN1 != IN2"
TKNUTH  FUNCTION="IN1 == IN2"
TKNUTH  FUNCTION="IN1 <= IN2"
write "The following strings should all result in BAD FUNCTION errors"
TKNUTH  FUNCTION="IN1.EQ./4"
TKNUTH  FUNCTION="3*("
TKNUTH  FUNCTION="(3,4)"
TKNUTH  FUNCTION="NONSENSE"
TKNUTH  FUNCTION="IN1+"
TKNUTH  FUNCTION=")"
TKNUTH  FUNCTION="IN1.EQ.*4"
! -- more bad functions, per SXP comments -NDR
TKNUTH  FUNCTION="ATAN7(1,2)"
TKNUTH  FUNCTION="SIN5"
TKNUTH  FUNCTION="MOD(0)"
TKNUTH  FUNCTION="MOD(IN1)"
TKNUTH  FUNCTION="ATAN2(IN1)"
TKNUTH  FUNCTION="ATAN2(1,2,3)"
TKNUTH  FUNCTION="3 2"
TKNUTH  FUNCTION="MOD(1,2,3)"
TKNUTH  FUNCTION="LOG 10,2"
TKNUTH  FUNCTION="JUNK" 'KNVAR

write "*** Tests on XKNUTH, KNUTH_LOOKUP ***"
TKNUTH  FUNCTION="IN1+IN2" 'XKNUTH
TKNUTH  FUNCTION="(IN1+1)/(IN2+1)" 'XKNUTH
TKNUTH  FUNCTION="min(int(IN1/IN2),999)" 'XKNUTH
TKNUTH  FUNCTION="ABS(IN1-IN2)" 'XKNUTH
TKNUTH  FUNCTION="Amod(IN1,IN2+1)" 'XKNUTH
TKNUTH  FUNCTION="1.5" 'XKNUTH
TKNUTH  FUNCTION="1.4" 'XKNUTH
write "*** Tests on KNUTH_VAR ***"
TKNUTH  FUNCTION="IN1" 'KNVAR
TKNUTH  FUNCTION="IN50" 'KNVAR
TKNUTH  FUNCTION="DN" 'KNVAR
TKNUTH  FUNCTION="LINE" 'KNVAR
TKNUTH  FUNCTION="SAMP" 'KNVAR
TKNUTH  FUNCTION="BAND" 'KNVAR
TKNUTH  FUNCTION="X1" 'KNVAR
TKNUTH  FUNCTION="C13" 'KNVAR
write "*** Test COMPLEX Operations ***"
TKNUTH  FUNCTION="IN1+IN2" 'XKNUTH 'COMPLEX CVALUES=(0,1,2,0)
TKNUTH  FUNCTION="IN1+IN2+IN3" 'XKNUTH 'COMPLEX CVALUES=(0,1,2,0,4,5)
TKNUTH  FUNCTION="IN1-IN2" 'XKNUTH 'COMPLEX CVALUES=(5,4,2,-1)
TKNUTH  FUNCTION="IN1*IN2" 'XKNUTH 'COMPLEX CVALUES=(1,1,1,1)
TKNUTH  FUNCTION="IN1/IN2" 'XKNUTH 'COMPLEX CVALUES=(0,2,1,1)
TKNUTH  FUNCTION="ABS(IN1)" 'XKNUTH 'COMPLEX CVALUES=(1,1)
TKNUTH  FUNCTION="SIN(IN1/4)" 'XKNUTH 'COMPLEX CVALUES=(3.14159,0)
TKNUTH  FUNCTION="COS(IN1/4)" 'XKNUTH 'COMPLEX CVALUES=(0,1)
TKNUTH  FUNCTION="TAN(IN1/4)" 'XKNUTH 'COMPLEX CVALUES=(3.14159,1)
TKNUTH  FUNCTION="3 + 4*SQRT(-1)" 'XKNUTH 'COMPLEX 
TKNUTH  FUNCTION="IN1 ** sqrt(-1)*IN2" 'XKNUTH 'COMPLEX +
   CVALUES=(2.71828,0,3.1416,0)
TKNUTH  FUNCTION="MOD(IN1,IN2)" 'XKNUTH 'COMPLEX CVALUES=(16,7,4,3)
TKNUTH  FUNCTION="MAX(IN1,IN2)" 'XKNUTH 'COMPLEX CVALUES=(16,2,4,3)
TKNUTH  FUNCTION="MIN(IN1,IN2)" 'XKNUTH 'COMPLEX CVALUES=(16,2,4,3)
TKNUTH  FUNCTION="IN1 == IN2" 'XKNUTH 'COMPLEX CVALUES=(16,16,4,3)
TKNUTH  FUNCTION="IN1 != IN2" 'XKNUTH 'COMPLEX CVALUES=(16,16,16,3)
write "This should return e (2.718) for COMPLEX"
tknuth fun="cos(in1) + sqrt(-1) * sin(in1)" 'xkn 'comp cval=(0,1)
write "This should return PI (3.1415) *i for COMPLEX"
TKNUTH  FUNCTION="2*LOG(IN1)" 'XKNUTH 'COMPLEX CVALUES=(0,1)

end-proc
$!-----------------------------------------------------------------------------
$ create tstknuth.log_solos
tstknuth
write "*** Test KNUTH, KNUTH_DUMP ***"
*** Test KNUTH, KNUTH_DUMP ***
TKNUTH
Beginning VICAR task TKNUTH
   LOAD   1
   RETN   0
TKNUTH  FUNCTION="LINE"
Beginning VICAR task TKNUTH
   LOAD  49
   RETN   0
TKNUTH  FUNCTION="SAMP"
Beginning VICAR task TKNUTH
   LOAD  50
   RETN   0
TKNUTH  FUNCTION="IN1+1"
Beginning VICAR task TKNUTH
   LOAD   1
   ADD   52
   RETN   0
TKNUTH  FUNCTION="SQRT(IN1**1.1234+7)"
Beginning VICAR task TKNUTH
   LOAD   1
   EXP   52
   ADD   53
   STOR  93
   SQRT  93
   RETN   0
TKNUTH  FUNCTION="SQRT(IN1**1.1234e1+7)"
Beginning VICAR task TKNUTH
   LOAD   1
   EXP   52
   ADD   53
   STOR  93
   SQRT  93
   RETN   0
TKNUTH  FUNCTION="IN1.AND.IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   AND    2
   RETN   0
TKNUTH  FUNCTION="IN1.OR.IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   OR     2
   RETN   0
TKNUTH  FUNCTION="IN1.XOR.IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   XOR    2
   RETN   0
TKNUTH  FUNCTION="IN1.LT.IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   LT     2
   RETN   0
TKNUTH  FUNCTION="IN1.GT.IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   GT     2
   RETN   0
TKNUTH  FUNCTION="IN1.LE.IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   LE     2
   RETN   0
TKNUTH  FUNCTION="IN1.EQ.IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   EQ     2
   RETN   0
TKNUTH  FUNCTION="IN1.EQ..5"
Beginning VICAR task TKNUTH
   LOAD   1
   EQ    52
   RETN   0
TKNUTH  FUNCTION="5..EQ.IN1"
Beginning VICAR task TKNUTH
   LOAD  52
   EQ     1
   RETN   0
TKNUTH  FUNCTION=".NOT.(IN1.GE.IN2)"
Beginning VICAR task TKNUTH
   LOAD   1
   GE     2
   STOR  93
   NOT   93
   RETN   0
TKNUTH  FUNCTION="MOD(IN2,IN1)"
Beginning VICAR task TKNUTH
   LOAD   2
   MOD    1
   RETN   0
TKNUTH  FUNCTION="MIN(IN1,IN2)"
Beginning VICAR task TKNUTH
   LOAD   1
   MIN    2
   RETN   0
TKNUTH  FUNCTION="MAX(IN1,IN2)"
Beginning VICAR task TKNUTH
   LOAD   1
   MAX    2
   RETN   0
TKNUTH  FUNCTION="MAX(COS(SIN(-IN1)+-3)/2,TAN(0))"
Beginning VICAR task TKNUTH
   LCMP   1
   STOR  93
   SIN   93
   STOR  95
   LCMP  52
   ADD   95
   STOR  97
   COS   97
   DIV   53
   STOR  99
   TAN   54
   MAX   99
   RETN   0
TKNUTH  FUNCTION="MAX(IN1,IN2,IN3,IN4,IN5)"
Beginning VICAR task TKNUTH
   LOAD   1
   MAX    2
   MAX    3
   MAX    4
   MAX    5
   RETN   0
TKNUTH  FUNCTION="100.*SIN(IN1/10.)+100.*COS(IN2/10)"
Beginning VICAR task TKNUTH
   LOAD   1
   DIV   53
   STOR  93
   SIN   93
   MUL   52
   STOR  95
   LOAD   2
   DIV   55
   STOR  97
   COS   97
   MUL   54
   ADD   95
   RETN   0
TKNUTH  FUNCTION="LINE+SAMP"
Beginning VICAR task TKNUTH
   LOAD  49
   ADD   50
   RETN   0
TKNUTH  FUNCTION="MOD(SAMP,LINE)"
Beginning VICAR task TKNUTH
   LOAD  50
   MOD   49
   RETN   0
write "*** Test C-language Constructs ***"
*** Test C-language Constructs ***
TKNUTH  FUNCTION="IN1 | IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   OR     2
   RETN   0
TKNUTH  FUNCTION="IN1 & IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   AND    2
   RETN   0
TKNUTH  FUNCTION="IN1 % IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   MOD    2
   RETN   0
TKNUTH  FUNCTION="IN1 ^ IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   XOR    2
   RETN   0
TKNUTH  FUNCTION="!IN1"
Beginning VICAR task TKNUTH
   NOT    1
   RETN   0
TKNUTH  FUNCTION="IN1 < IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   LT     2
   RETN   0
TKNUTH  FUNCTION="IN1 > IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   GT     2
   RETN   0
TKNUTH  FUNCTION="IN1 != IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   NE     2
   RETN   0
TKNUTH  FUNCTION="IN1 == IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   EQ     2
   RETN   0
TKNUTH  FUNCTION="IN1 <= IN2"
Beginning VICAR task TKNUTH
   LOAD   1
   LE     2
   RETN   0
write "The following strings should all result in BAD FUNCTION errors"
The following strings should all result in BAD FUNCTION errors
TKNUTH  FUNCTION="IN1.EQ./4"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="3*("
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="(3,4)"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="NONSENSE"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="IN1+"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION=")"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="IN1.EQ.*4"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="ATAN7(1,2)"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="SIN5"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="MOD(0)"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="MOD(IN1)"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="ATAN2(IN1)"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="ATAN2(1,2,3)"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="3 2"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="MOD(1,2,3)"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="LOG 10,2"
Beginning VICAR task TKNUTH
BAD FUNCTION STRING
 ** ABEND called **
continue
TKNUTH  FUNCTION="JUNK" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "JUNK " INDEX= -1
BAD FUNCTION STRING
 ** ABEND called **
continue
write "*** Tests on XKNUTH, KNUTH_LOOKUP ***"
*** Tests on XKNUTH, KNUTH_LOOKUP ***
TKNUTH  FUNCTION="IN1+IN2" 'XKNUTH
Beginning VICAR task TKNUTH
   LOAD   1
   ADD    2
   RETN   0
FUNCTION VALUES:
    XKNUTH(   0.0,   0.0)=   0.0
    XKNUTH(   0.0,   2.0)=   2.0
    XKNUTH(   3.0,   0.0)=   3.0
    XKNUTH(   3.0,   2.0)=   5.0
    XKNUTH( 130.0, 255.0)= 385.0
LOOKUP TABLE VALUES (trunc.,int indx):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   2
    TABLE(   3,   0)=   3
    TABLE(   3,   2)=   5
LOOKUP TABLE VALUE (trunc,byte indx):
    TABLE( 130, 255)= 255
LOOKUP TABLE VALUES (round):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   2
    TABLE(   3,   0)=   3
    TABLE(   3,   2)=   5
LOOKUP TABLE VALUE (round,byte index):
    TABLE( 130, 255)= 255
TKNUTH  FUNCTION="(IN1+1)/(IN2+1)" 'XKNUTH
Beginning VICAR task TKNUTH
   LOAD   1
   ADD   52
   STOR  93
   LOAD   2
   ADD   53
   STOR  95
   LOAD  93
   DIV   95
   RETN   0
FUNCTION VALUES:
    XKNUTH(   0.0,   0.0)=   1.0
    XKNUTH(   0.0,   2.0)=   0.3
    XKNUTH(   3.0,   0.0)=   4.0
    XKNUTH(   3.0,   2.0)=   1.3
    XKNUTH( 130.0, 255.0)=   0.5
LOOKUP TABLE VALUES (trunc.,int indx):
    TABLE(   0,   0)=   1
    TABLE(   0,   2)=   0
    TABLE(   3,   0)=   4
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (trunc,byte indx):
    TABLE( 130, 255)=   0
LOOKUP TABLE VALUES (round):
    TABLE(   0,   0)=   1
    TABLE(   0,   2)=   0
    TABLE(   3,   0)=   4
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (round,byte index):
    TABLE( 130, 255)=   1
TKNUTH  FUNCTION="min(int(IN1/IN2),999)" 'XKNUTH
Beginning VICAR task TKNUTH
   LOAD   1
   DIV    2
   STOR  93
   INT   93
   MIN   52
   RETN   0
FUNCTION VALUES:
    XKNUTH(   0.0,   0.0)=   0.0
    XKNUTH(   0.0,   2.0)=   0.0
    XKNUTH(   3.0,   0.0)= 999.0
    XKNUTH(   3.0,   2.0)=   1.0
    XKNUTH( 130.0, 255.0)=   0.0
LOOKUP TABLE VALUES (trunc.,int indx):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   0
    TABLE(   3,   0)= 255
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (trunc,byte indx):
    TABLE( 130, 255)=   0
LOOKUP TABLE VALUES (round):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   0
    TABLE(   3,   0)= 255
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (round,byte index):
    TABLE( 130, 255)=   0
TKNUTH  FUNCTION="ABS(IN1-IN2)" 'XKNUTH
Beginning VICAR task TKNUTH
   LOAD   1
   SUB    2
   STOR  93
   ABS   93
   RETN   0
FUNCTION VALUES:
    XKNUTH(   0.0,   0.0)=   0.0
    XKNUTH(   0.0,   2.0)=   2.0
    XKNUTH(   3.0,   0.0)=   3.0
    XKNUTH(   3.0,   2.0)=   1.0
    XKNUTH( 130.0, 255.0)= 125.0
LOOKUP TABLE VALUES (trunc.,int indx):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   2
    TABLE(   3,   0)=   3
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (trunc,byte indx):
    TABLE( 130, 255)= 125
LOOKUP TABLE VALUES (round):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   2
    TABLE(   3,   0)=   3
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (round,byte index):
    TABLE( 130, 255)= 125
TKNUTH  FUNCTION="Amod(IN1,IN2+1)" 'XKNUTH
Beginning VICAR task TKNUTH
   LOAD   2
   ADD   52
   STOR  93
   LOAD   1
   MOD   93
   RETN   0
FUNCTION VALUES:
    XKNUTH(   0.0,   0.0)=   0.0
    XKNUTH(   0.0,   2.0)=   0.0
    XKNUTH(   3.0,   0.0)=   0.0
    XKNUTH(   3.0,   2.0)=   0.0
    XKNUTH( 130.0, 255.0)= 130.0
LOOKUP TABLE VALUES (trunc.,int indx):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   0
    TABLE(   3,   0)=   0
    TABLE(   3,   2)=   0
LOOKUP TABLE VALUE (trunc,byte indx):
    TABLE( 130, 255)= 130
LOOKUP TABLE VALUES (round):
    TABLE(   0,   0)=   0
    TABLE(   0,   2)=   0
    TABLE(   3,   0)=   0
    TABLE(   3,   2)=   0
LOOKUP TABLE VALUE (round,byte index):
    TABLE( 130, 255)= 130
TKNUTH  FUNCTION="1.5" 'XKNUTH
Beginning VICAR task TKNUTH
   LOAD  52
   RETN   0
FUNCTION VALUES:
    XKNUTH(   0.0,   0.0)=   1.5
    XKNUTH(   0.0,   2.0)=   1.5
    XKNUTH(   3.0,   0.0)=   1.5
    XKNUTH(   3.0,   2.0)=   1.5
    XKNUTH( 130.0, 255.0)=   1.5
LOOKUP TABLE VALUES (trunc.,int indx):
    TABLE(   0,   0)=   1
    TABLE(   0,   2)=   1
    TABLE(   3,   0)=   1
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (trunc,byte indx):
    TABLE( 130, 255)=   1
LOOKUP TABLE VALUES (round):
    TABLE(   0,   0)=   2
    TABLE(   0,   2)=   2
    TABLE(   3,   0)=   2
    TABLE(   3,   2)=   2
LOOKUP TABLE VALUE (round,byte index):
    TABLE( 130, 255)=   2
TKNUTH  FUNCTION="1.4" 'XKNUTH
Beginning VICAR task TKNUTH
   LOAD  52
   RETN   0
FUNCTION VALUES:
    XKNUTH(   0.0,   0.0)=   1.4
    XKNUTH(   0.0,   2.0)=   1.4
    XKNUTH(   3.0,   0.0)=   1.4
    XKNUTH(   3.0,   2.0)=   1.4
    XKNUTH( 130.0, 255.0)=   1.4
LOOKUP TABLE VALUES (trunc.,int indx):
    TABLE(   0,   0)=   1
    TABLE(   0,   2)=   1
    TABLE(   3,   0)=   1
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (trunc,byte indx):
    TABLE( 130, 255)=   1
LOOKUP TABLE VALUES (round):
    TABLE(   0,   0)=   1
    TABLE(   0,   2)=   1
    TABLE(   3,   0)=   1
    TABLE(   3,   2)=   1
LOOKUP TABLE VALUE (round,byte index):
    TABLE( 130, 255)=   1
write "*** Tests on KNUTH_VAR ***"
*** Tests on KNUTH_VAR ***
TKNUTH  FUNCTION="IN1" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "IN1  " INDEX=  1
   LOAD   1
   RETN   0
TKNUTH  FUNCTION="IN50" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "IN50 " INDEX= 50
   LOAD  50
   RETN   0
TKNUTH  FUNCTION="DN" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "DN   " INDEX=  1
   LOAD   1
   RETN   0
TKNUTH  FUNCTION="LINE" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "LINE " INDEX= 49
   LOAD  49
   RETN   0
TKNUTH  FUNCTION="SAMP" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "SAMP " INDEX= 50
   LOAD  50
   RETN   0
TKNUTH  FUNCTION="BAND" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "BAND " INDEX= 51
   LOAD  51
   RETN   0
TKNUTH  FUNCTION="X1" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "X1   " INDEX=  1
   LOAD   1
   RETN   0
TKNUTH  FUNCTION="C13" 'KNVAR
Beginning VICAR task TKNUTH
VARIABLE "C13  " INDEX= 13
   LOAD  13
   RETN   0
write "*** Test COMPLEX Operations ***"
*** Test COMPLEX Operations ***
TKNUTH  FUNCTION="IN1+IN2" 'XKNUTH 'COMPLEX CVALUES=(0,1,2,0)
Beginning VICAR task TKNUTH
   LOAD   1
   ADD    2
   RETN   0
COMPLEX-RESULT=  2.000  1.000
REAL-RESULT=  1.000
TKNUTH  FUNCTION="IN1+IN2+IN3" 'XKNUTH 'COMPLEX CVALUES=(0,1,2,0,4,5)
Beginning VICAR task TKNUTH
   LOAD   1
   ADD    2
   ADD    3
   RETN   0
COMPLEX-RESULT=  6.000  6.000
REAL-RESULT=  3.000
TKNUTH  FUNCTION="IN1-IN2" 'XKNUTH 'COMPLEX CVALUES=(5,4,2,-1)
Beginning VICAR task TKNUTH
   LOAD   1
   SUB    2
   RETN   0
COMPLEX-RESULT=  3.000  5.000
REAL-RESULT=  1.000
TKNUTH  FUNCTION="IN1*IN2" 'XKNUTH 'COMPLEX CVALUES=(1,1,1,1)
Beginning VICAR task TKNUTH
   LOAD   1
   MUL    2
   RETN   0
COMPLEX-RESULT=  0.000  2.000
REAL-RESULT=  1.000
TKNUTH  FUNCTION="IN1/IN2" 'XKNUTH 'COMPLEX CVALUES=(0,2,1,1)
Beginning VICAR task TKNUTH
   LOAD   1
   DIV    2
   RETN   0
COMPLEX-RESULT=  1.000  1.000
REAL-RESULT=  0.000
TKNUTH  FUNCTION="ABS(IN1)" 'XKNUTH 'COMPLEX CVALUES=(1,1)
Beginning VICAR task TKNUTH
   ABS    1
   RETN   0
COMPLEX-RESULT=  1.414  0.000
REAL-RESULT=  1.000
TKNUTH  FUNCTION="SIN(IN1/4)" 'XKNUTH 'COMPLEX CVALUES=(3.14159,0)
Beginning VICAR task TKNUTH
   LOAD   1
   DIV   52
   STOR  93
   SIN   93
   RETN   0
COMPLEX-RESULT=  0.707  0.000
REAL-RESULT=  0.707
TKNUTH  FUNCTION="COS(IN1/4)" 'XKNUTH 'COMPLEX CVALUES=(0,1)
Beginning VICAR task TKNUTH
   LOAD   1
   DIV   52
   STOR  93
   COS   93
   RETN   0
COMPLEX-RESULT=  1.031  0.000
REAL-RESULT=  1.000
TKNUTH  FUNCTION="TAN(IN1/4)" 'XKNUTH 'COMPLEX CVALUES=(3.14159,1)
Beginning VICAR task TKNUTH
   LOAD   1
   DIV   52
   STOR  93
   TAN   93
   RETN   0
COMPLEX-RESULT=  1.000  0.000
REAL-RESULT=  1.000
TKNUTH  FUNCTION="3 + 4*SQRT(-1)" 'XKNUTH 'COMPLEX
Beginning VICAR task TKNUTH
   LCMP  54
   STOR  93
   SQRT  93
   MUL   53
   ADD   52
   RETN   0
COMPLEX-RESULT=  3.000  4.000
REAL-RESULT=  7.000
TKNUTH  FUNCTION="IN1 ** sqrt(-1)*IN2" 'XKNUTH 'COMPLEX  +
   CVALUES=(2.71828,0,3.1416,0)
Beginning VICAR task TKNUTH
   LCMP  52
   STOR  93
   SQRT  93
   STOR  95
   LOAD   1
   EXP   95
   MUL    2
   RETN   0
COMPLEX-RESULT=  1.697  2.644
REAL-RESULT=  0.000
TKNUTH  FUNCTION="MOD(IN1,IN2)" 'XKNUTH 'COMPLEX CVALUES=(16,7,4,3)
Beginning VICAR task TKNUTH
   LOAD   1
   MOD    2
   RETN   0
COMPLEX-RESULT=  0.000  1.000
REAL-RESULT=  2.000
TKNUTH  FUNCTION="MAX(IN1,IN2)" 'XKNUTH 'COMPLEX CVALUES=(16,2,4,3)
Beginning VICAR task TKNUTH
   LOAD   1
   MAX    2
   RETN   0
COMPLEX-RESULT= 16.000  3.000
REAL-RESULT= 16.000
TKNUTH  FUNCTION="MIN(IN1,IN2)" 'XKNUTH 'COMPLEX CVALUES=(16,2,4,3)
Beginning VICAR task TKNUTH
   LOAD   1
   MIN    2
   RETN   0
COMPLEX-RESULT=  4.000  2.000
REAL-RESULT=  2.000
TKNUTH  FUNCTION="IN1 == IN2" 'XKNUTH 'COMPLEX CVALUES=(16,16,4,3)
Beginning VICAR task TKNUTH
   LOAD   1
   EQ     2
   RETN   0
COMPLEX-RESULT=  0.000  0.000
REAL-RESULT=  1.000
TKNUTH  FUNCTION="IN1 != IN2" 'XKNUTH 'COMPLEX CVALUES=(16,16,16,3)
Beginning VICAR task TKNUTH
   LOAD   1
   NE     2
   RETN   0
COMPLEX-RESULT=  1.000  0.000
REAL-RESULT=  0.000
write "This should return e (2.718) for COMPLEX"
This should return e (2.718) for COMPLEX
tknuth fun="cos(in1) + sqrt(-1) * sin(in1)" 'xkn 'comp cval=(0,1)
Beginning VICAR task tknuth
   COS    1
   STOR  93
   LCMP  52
   STOR  95
   SQRT  95
   STOR  97
   SIN    1
   MUL   97
   ADD   93
   RETN   0
COMPLEX-RESULT=  2.718  0.000
REAL-RESULT=  1.000
write "This should return PI (3.1415) *i for COMPLEX"
This should return PI (3.1415) *i for COMPLEX
TKNUTH  FUNCTION="2*LOG(IN1)" 'XKNUTH 'COMPLEX CVALUES=(0,1)
Beginning VICAR task TKNUTH
   LN     1
   MUL   52
   RETN   0
COMPLEX-RESULT=  0.000  3.142
REAL-RESULT=-92.103
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC

! 11-Feb-2007:  the test log on Linux is identical to this one (lwk)
$ Return
$!#############################################################################
$Other_File:
$ create knuth.hlp
1  XKNUTH

     XKNUTH, XKNUTH_COMPLEX

     Two routines that provide the ability to dynamically compile
     and execute FORTRAN IV or C - like expressions (see KNUTH),
	 using either REAL or COMPLEX arithmetic.

2  CALLING SEQUENCE
 
    FORTRAN:
	         REAL*4 BUF(300),RESULT
			 COMPLEX*8 CBUF(150),CRESULT
			 
             CALL XKNUTH (BUF,RESULT)
             CALL XKNUTH_COMPLEX (CBUF,CRESULT)

    C:
			float buf[300],result,cresult[2];
            zxknuth(buf,&result);
			zxknuth_complex(buf,cresult);
    
2  ARGUMENTS
     BUF      is a 300 word(4 bytes each) array which holds the compiled
              expression and variable values. If XKNUTH_COMPLEX is called,
			  it contains 300 COMPLEX value-pairs (and was compiled with
			  KNUTH_COMPLEX).

     RESULT   is a REAL*4 output which is set to the expression value
              upon return. If XKNUTH_COMPLEX is called, RESULT should
			  be COMPLEX (or float[2] in C).
              
1  KNUTH_LOOKUP

     A routine that provides the ability to compute FORTRAN IV or 
     C - like expressions and store values in a lookup table (See KNUTH).
	 It does not handle complex-valued expressions, as does XKNUTH_COMPLEX.

2  CALLING SEQUENCE
 
    FORTRAN:
             CALL KNUTH_LOOKUP(BUF,TABLE,NUM_INP,FLAGS)

    C:
             zknuth_lookup(buf,table,num_inp,flags);

    
2  ARGUMENTS

     BUF      is a 300 word(4 bytes each) array which holds the compiled
              expression and variable values. BUF must have been compiled
			  with KNUTH, and not KNUTH_COMPLEX for this code to work.

     TABLE    is a BYTE (unsigned char) output array containing a
              lookup table for one or two BYTE inputs

     NUM_INP  is an INTEGER*4 input indicating whether the table should
              be a lookup table for one or two input byte values.

     FLAGS    is an INTEGER containing bit-field flags indicating how
              knuth_lookup should build the lookup table:

              bit 1: table values should be derived from rounding
              (bit=1) or truncating (bit=0) the REAL*4 values.
              bit 2: table is indexed using FORTRAN signed BYTE values
              (ie, index is -128:127) if bit=1, else index is (0:255).

              For example, FLAGS=3 indicates use of rounding and signed
              byte values.

1  KNUTH_DUMP

     A routine that provides a symbolic dump ("F2-style") of a buffer of
     compiled code, executable by knuth (See KNUTH). Note: This code
	 will not work with a buffer compiled with KNUTH_COMPLEX. However,
	 since the same compiler module is used for both routines, you
	 can generate its symbolic dump by passing the original string
	 to KNUTH, creating a temporary REAL buffer, and then dumping
	 that buffer instead.

2  CALLING SEQUENCE
 
    FORTRAN:
             CALL KNUTH_DUMP(BUF)

    C:
             zknuth_dump(buf);

    
2  EXAMPLE

   In the code segment:

         REAL*4 BUF(300)
		 INTEGER*4 IER
         CALL KNUTH('IN1+20*LINE',BUF,IER)
         CALL KNUTH_DUMP(BUF) 
     
   The subroutine KNUTH_DUMP will output the following
   sequence (or similar) of operations compiled in the buffer:
   
           LOAD  22
           MUL   19
           ADD    1
           RETN   0

1  KNUTH_VAR

    KNUTH_VAR, zknuth_var:
    
     A routine that returns the buffer index of a named variable
     recognized by the expression compiler knuth (See KNUTH). The
     routines return zero-based or one-based indices for C and
     FORTRAN arrays, respectively. Special case: If you are using
	 XKNUTH_COMPLEX operations, then all indices refer to indices
	 in the COMPLEX array (or in the float[2] array for C).

2  CALLING SEQUENCE
 
    FORTRAN:
             CALL KNUTH_VAR(NAME,INDEX)

    C:
             index=zknuth_var(name);

    
2  ARGUMENTS

     NAME      is a CHARACTER*N (or C-string) ascii string input,
               specifying the variable name
               
     INDEX     is and INTEGER*4 (int) output value, returning
               the buffer index (or -1 if not recognized). The
               value ,if valid, will be "zero-based" for C and 
               "one-based"for FORTRAN arrays.

2  EXAMPLES

3 FORTRAN

     KNUTH_VAR returns a "one-based" index, which will be correct
     for standard FORTRAN arrays:
    
     INTEGER*4 IER,LINEINDEX
     REAL*4 BUF(300), RESULT
     CALL KNUTH('IN1+LINE',BUF,IER)
     CALL KNUTH_VAR('LINE',LINEINDEX) !Gets index of "line"
     BUF(1) = 2.0
     BUF(LINEINDEX) = 3.0
     CALL XKNUTH (BUF,RESULT)
     
     < The value of result will be (IN1+LINE) = (2.0 + 3.0) = 5.0 >

3 C-LANGUAGE
 
    zknuth_var returns the correct index for a "zero-based"
    array in C:
    
     {
         int ier,dn_index,line_index;
         float buf[300],result;
         
         zknuth( "dn % line", buf, &ier);
         dn_index=zknuth_var("dn");
         line_index=zknuth_var("line");
         buf[dn_index] = 10;
         buf[line_index]=7;   
         zxknuth( buf, &result);
         ...
     }
    
     The value of result is = (dn % line) = (10 % 7)  = 3.0.

1  KNUTH

     KNUTH, KNUTH_COMPLEX, XKNUTH, XKNUTH_COMPLEX, KNUTH_LOOKUP, 
	 KNUTH_VAR, KNUTH_DUMP:
     
     A suite of routines that provide the ability to dynamically
     compile and execute FORTRAN IV or C - like expressions, using
	 real or complex arithmetic, and then optionally,
     to store the values into a byte lookup table.

2  CALLING SEQUENCE
 
    FORTRAN:    
            CALL KNUTH (STRING,BUF,IER)
            CALL KNUTH_COMPLEX (STRING,BUF,IER)

    C:
            zknuth(string,buf,&ier);
            zknuth_complex(string,buf,&ier);
    
2  ARGUMENTS
     STRING   is a CHARACTER*N (char[]) input containing an arithmetic
               expression in ASCII,optionally ending in '$'.  Need not 
              be uppercase.

     BUF      is a 300 word(4 bytes each) array which holds the compiled
              expression and variable values. For KNUTH_COMPLEX this is
			  an array of 300 COMPLEX*8 (float[2]) values.

     IER      is an INTEGER*4 output which allows for warnings and
              error codes. IER=1 warns that variables "LINE", "SAMP"
              or "BAND" were referenced in the compilation.
              IER=2 indicates a bad function string and IER=3 is an 
              evaluation error. 0 indicates no warnings or errors.


2  OPERATION

      These routines give the ability to dynamically compile and
    execute FORTRAN and C - like expressions.  The expression is passed to
    routine KNUTH as a character string and compiled into a buffer area.
    It is the responsibility of the calling program to place input values
    (and line,samp,band, if required) into the BUF array. A subsequent call
    to XKNUTH calculates the value of the expression as a function of its
    variables. Alternatively, the subroutine KNUTH_LOOKUP will produce a 
    byte lookup table for one or two byte-valued inputs.
    
    The variables are placed in a vector which is part of the buffer area.
	For KNUTH the vector is REAL*4, while for KNUTH_COMPLEX they are COMPLEX.
    The arithmetic operation is specified by a FORTRAN or C-like expression
    which can include the following operators (synonyms connected by an =):
    
        + - * / ** unary-
	LOG   = ALOG    =  LN  (all these = natural (base e) log)
	LOG10 = ALOG10         (all these = common (base 10) log)
        SQRT   ABS   AINT = INT
        SIN    COS  TAN    ASIN  ACOS  ATAN  ATAN2
        AMAX1 = MAX  AMIN1 = MIN
        AMOD   MOD .AND.  .OR.  .XOR.  .NOT.
        .LT.  .LE. .EQ.   .NE.  .GT.  .GE.
    
    as well as the following C-operators:
    
        &    &&   |    ||    %   ^   !  
        ==   !=   >=   <=   >   <   >>   <<
     
	 (KNUTH_COMPLEX supports all of these except ASIN,ACOS and ATAN).
	 
     All operations function as in their respective languages including
     MIN and MAX, which may now accept two or more arguments. However,
     for historical reasons the FORTRAN logical operators act bitwise on
     the 4-byte integer equivalent of the truncated floating point value.

     Note that one must be careful, if using the '&' character, to put a
     space between it and the next argument, or else the VICAR executive
     will try to interpret that argument as the name of a VICAR variable.
     Also, the VICAR executive interprets "&&" as a single '&', so 
     logical-AND must be doubly-escaped: "7 &&&& 5".

     Historically, KNUTH used a standard technique (see REFERENCES) to
     convert ordinary  arithmetic expressions into polish notation. Since
     then the code has been completely rewritten in C as a recursive 
     expression parser.  The parser performs a certain amount of 
     optimization, taking advantage of the fact that some operations do 
     not depend upon argument order (such as +,*,MAX, etc).

     KNUTH recognizes a number of variable names and associates them with
     certain locations in the BUF array. The following table shows the
     currently recognized names and the BUF index associated with it:
     
            NAME            INDEX
            ---------       ---------
            IN1 ... IN50    1 ... 50
            X1 ... X50      1 ... 50  (where "X" is any letter except
            DN              1                            i,d,l,s,or b)
            LINE            *
            SAMP            *
            BAND            *
     
     The indices marked (*) are implementation dependent; to find which 
     buffer index is assigned to this or any other variable, use the 
     KNUTH_VAR subroutine. If the variables LINE, SAMP or BAND are 
     referenced in the expression, this will be flagged by KNUTH by 
     setting the IER flag to 1.

     Because of the type of calculations done on images, the routine XKNUTH
     is designed to never abort regardless of input values.  Illegal 
     operations result in the generation of a "reasonable" result as 
     follows: a) divide by zero becomes divide by 1.E-20;  b) log of a
     negative number becomes log of its absolute value;  c) int() of a
     number outside the range (-2147483648,2147483647) becomes the closest
     value in that range (which is that of a 32-bit integer, so any extra
     precision on a machine where an int has more bits is lost); etc.

     The routine KNUTH quits processing, prints an error message and
     returns if a syntax error is detected in the expression string.
     The same holds true for KNUTH_COMPLEX and XKNUTH_COMPLEX, except
     that some "illegal" operations have meaningful values in the complex
     domain: for example, the log of a negative number is simply the log
     of the positive value plus PI*(i), where i is the imaginary square
     root of (-1).

3  TECHNICAL-DETAILS

4  OVERVIEW

    The knuth subroutine is now basically a FORTRAN bridge to the
    C - language subroutine zknuth, which is a recursive expression
    parser and compiler. The bridge routine converts the FORTRAN
    string to C format and passes it on.
    
    The parser first calls the routine initialize_parser,
    which checks for closed parentheses, converts the string to
    all UPPERCASE for simplicity, and eliminates any terminating
    '$' characters (for backwards compatibility).
    
    The subroutine then calls the main entry to the recursive parsing
    loop, called "compile_loop", which parses and compiles the
    expression string into a register-based binary, executable by the
    subroutine "xknuth", which is a FORTRAN bridge to the C-language 
    version zxknuth.


4 STACKS/REGISTERS

    Stack-Based  vs. Register-based computations
    
    On the machine level of most computers today, computations are
    performed one step at a time, where each operation is of the form
    
                    <OPERATION> <ADDRESSES>
                    
    and uses a fixed set of registers, one of which is set as the
    active register.For example, the machine implementation of
    "Z=X+Y" may be (in pseudo-assembly):
    
                 LOAD X        ; Load value at X into active register
                 ADD  Y        ; Add value at Y into register
                 STOR Z        ; Store register value into Z.
    
    However, when scanning through a complex math expression,it is
    often easier to have a way of "putting off for later" 
    intermediate expressions which are to be combined in the end. 
    For this purpose a structure called a Stack may be implemented, 
    which is an area of memory where numbers may
    be "pushed" on and "popped" off (This is how most hand-held
    calculators operate -- even the non-Hewlett-Packard type!).
    
    For example, the expression:
    
            (X*Y) + (Z*W)
            
    may be implemented on a stack by:
    
            PUSH X            ; stack= X
            PUSH Y            ; stack= Y  X
            MULTIPLY          ; stack= (X*Y)
            PUSH Z            ; stack= Z   (X*Y)
            PUSH W            ; stack= W   Z   (X*Y)
            MULTIPLY          ; stack= (W*Z)   (X*Y)
            ADD               ; stack= (X*Y)+(W*Z)
    


4 PARSING
    The knuth parser really has two independent parts: a 
    Stack-based parser and a set of Stack-to-Register 
    operation converters.
    
    The parser is a set of token (such as "(", "*", or ".LT.")
    handling subroutines, all of which have the general schematic
    form:
    
        HANDLER_Y:
    
                Execute HANDLER_X
                
                while TOKEN is in HANDLER_Y_TOKENS
                
                    Get a new TOKEN
                    
                    Execute HANDLER_X
                    
                    ( Perform a computational ACTION_Y)
    
                end-while
                
    If knuth was an expression-evaluation, rather than expression-
    compiling, subroutine, the ACTION_Y in the schematic above would
    be something like
    
        ACTION_Y = "Take the numbers stored in the stack and perform 
        the Y_TOKEN operation given by TOKEN"
        
    However, since we actually want a register-based "executable"
    file of operations, the ACTION_Y  subroutines have the form:
    
        ACTION_Y = "Take the numbers stored in the stack and generate in
        the executable buffer the sequence of register commands
        needed to perform the Y_TOKEN operation given by TOKEN"
        
        
    For the current implementation of KNUTH, here is a table of the 
    token handlers, (HANDLER_Y), together with their set of
    HANDLER_Y_TOKENS, and the associated action item ACTION_Y. Each 
    handler calls the next on the list:
    

    HANDLER_Y        HANDLER_Y_TOKENS      ACTION_Y      Comments    
    
     commas                    ,        Count #args   Handles x,y,z,...
     logical1        .OR. || .XOR....   binary(op)    Simple logicals
     logical2        .AND. &&           binary(op)    Higher logicals
     compares        .LT. < >=...       binary(op)    Comparison ops
     shifts            >>  <<           binary(op)    bit shift ops
     arith1            + -              binary(op)    x+y arithmetic
     arith2            *  /  %          binary(op)    higher arithmetic
     exponents         **               binary(op)    exponentiation
     unary_arith       + -              unary(op)     +(x) or -(x)
     transcend       LN, SIN,SQRT...    unary(op)     transcendentals
     prefixes        MAX, ATAN2...      multi(op)     any OP(x,y,...)
     parentheses         ( )            primitive     grouping

    The subroutine "parentheses", in turn, calls commas, starting 
    the recursion loop.
    
    Each handler, before it grabs a token, makes sure that the next 
    highest handler gets first chance to grab the token before it takes
    the leftovers. After grabbing all the tokens the handler needs, it
    calls the action procedure.
    
    The action procedures binary, unary, multi and primitive, all 
    manipulate the stack of stored constants and variables with 
    various calls to pop() and push(), and also convert the requested
    operation to an equivalent sequence of register based operations, 
    using the subroutine stack_to_register(). This routine tries to do 
    some optimization, by checking the operation requested among its 
    list of order-independent (commutative) operations, and shuffling 
    the order of loads.
    
    For example, in the expression
    
                        X-Y*Z
                        
    if the variables will be put into the stack in the order they
    appear, to implement the register-based operation "-", which is
    order dependent, some temporary registers must be used, e.g.
    
                    LOAD X
                    STOR W
                    LOAD Y
                    MULT Z
                    STOR Q
                    LOAD W
                    SUB  Q
                    RETN
                    
    Even though the (STOR W,LOAD W) commands are not absolutely 
    necessary, ie:
    
                    LOAD Y
                    MULT Z
                    STOR Q
                    LOAD X
                    SUB  Q
                    RETN
                    
    will also do the job.
    
    In addition, the analogous sequence for, say,
    
                    X+Y*Z
                    
    may take advantage of the commutativity of addition, to become:
    
                    LOAD Y
                    MULT Z
                    ADD  X
                    RETN
                    
    These are the sequences that are generated by stack_to_register.            
                
2  EXAMPLES

3 FORTRAN

    
     BYTE TABLE(0:255, 0:255), DN   !Note the index starts at 0 !
     INTEGER*4 IER
     REAL*4 BUF(300), RESULT
     CALL KNUTH('IN1+SQRT(IN2)',BUF,IER)
     BUF(1) = 2.0
     BUF(2) = 9.0
     CALL XKNUTH (BUF,RESULT)
     ...
     ( The value returned in RESULT will be 2.0 + SQRT(9.0) = 5.0.)
     
     CALL KNUTH_LOOKUP(BUF,TABLE,1,0)
     
     DN = TABLE( 3, 17)
     
     ( The value of DN will be truncate( 3+SQRT(17) ) = 7. )

3 COMPLEX_FORTRAN

     INTEGER*4 IER
     COMPLEX*8 BUF(300), RESULT
     CALL KNUTH_COMPLEX('IN1+SQRT(IN2)',BUF,IER)
     BUF(1) = (2.0,0.0)   !2 + 0i
     BUF(2) = (-1.0,0.0)  !-1 + 0i
     CALL XKNUTH_COMPLEX (BUF,RESULT)
     ...
     ( The value returned in RESULT will be 2.0 + SQRT(-1) = 2+i)

3 C-LANGUAGE
 
     {
         int ier;
         float buf[300],result;
         
         zknuth( "dn % x2", buf, &ier);
         buf[0] = 10; buf[1]=7;        /* DN=buf[0] and x2=buf[1] */
         zxknuth( buf, &result);
         zknuth_lookup(buf,
         ...
     }
    
     The value returned in result is = (10 % 7) = (10 MOD 7 ) = 3.0.

3 COMPLEX-C
 
     {
         int ier;
         float buf[300][2],result[2];
         
         zknuth_complex( "log(dn) + x2", buf, &ier);
         buf[0][0] = -1; buf[0][1] = 0;      /* DN=-1 + 0i */
         buf[0][0] = 2; buf[0][1] = 0;      /* x2=-1 + 0i */
         zxknuth_complex( buf, result);
     }
    
     The value-pair returned in result is = log(-1)+2 = {2, 3.14159}.
     
     

2  TIMING

     Execution time of KNUTH is proportional to the length of the string.
     Execution time of XKNUTH is proportional to the time it would take
     compiled code to execute the expression except for an overhead due to 
     initializing machine instructions.  Thus, expressions involving only 
     + - * /, etc., are about 2 to 3 times slower, and those involving sin,
     sqrt, etc., are about 5 times slower.

2  RESTRICTIONS

     The expression must not contain more than 40 constants, 20 variables, 
     or roughly 80 operations.  Embedded blanks are allowed.

     When performing  arithmetic, KNUTH converts all integer operands to
     real and executes  single precision floating point computations.  The
     exception is  .AND., .OR., and .XOR. which operate bitwise on the
     integer equivalent of the truncated floating point number.  The final 
     result is converted back to integer by rounding, which is the default.
     The logical values "TRUE'  and 'FALSE' when produced, are interpreted
     as 1 and 0 respectively. The .NOT. operator only  operates on the
     logical values 0 and 1. 

     A subtle consequence of KNUTH converting integers to floating point is
     that exponentiation does not work as expected for negative values
     raised to an integer exponent.  This is because they are interpreted
     as negative values raised to a floating point exponent, which are
     undefined (not real) values, and so their positive magnitude is
     returned.  Odd powers can be handled using multiplication and the 
     next smaller even power. KNUTH_COMPLEX does not have these limitations.

2  REFERENCE

     KNUTH, D.E., "A History of Writing Compilers,"
     Computers and Automation, December, 1962, pp. 8-18.

     Schildt, H.S., "C: The Complete Reference,", Osborne McGraw-Hill,
     1987, pp. 547-570.
     
2  HISTORY

     KNUTH, XKNUTH
     Original Programmer: A. L. Zobrist, 1 September 1975
     Current Cognizant Programmer: A. L. Zobrist
     Documentation Authors: A. L. Zobrist, N. D. Ritter
     Revision: 1, (30 July 1984)
               2, (05 September 1990) version 9.05.90
                   Error checking of function expanded to include a
                   double operator check.
               3, (25 January 1991) version 01.25.91
                   Clean up for test files corrected.
               4, (05 August 1991) version 08.05.91
                   Completely new code in C; UNIX compatible;
                   recursive parser routines; handles C constructs
                   and performs optimization on compiles.
               5, (12 November 1994) version 11.12.94
                   Added KNUTH_COMPLEX, XKNUTH_COMPLEX for 
                   complex arithmetic & transcendental operations.
               12-Oct-2006 -lwk- changed SMALL from 1.e-6 to 1.e-20
               08-Feb-2007 -lwk- added code to check on the INT function.
$ Return
$!#############################################################################
