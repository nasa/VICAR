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

