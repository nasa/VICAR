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

