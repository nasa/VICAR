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

