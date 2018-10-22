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
#define MAXCODE 1000
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

