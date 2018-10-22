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
/*     AUG  15   WLB   INCREASED MAXCODE FROM 100 TO 1000 TO    */
/*                     HANDLE LONGER F2 FUNCTION STRINGS        */
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

