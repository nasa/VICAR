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
