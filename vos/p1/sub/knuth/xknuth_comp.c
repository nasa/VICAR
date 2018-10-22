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




