/*  This is the gtprcs subroutine for all Operating Systems.
    This subroutine returns a pointer to the current user's real 
    name.  If the user's real name cannot be found then a blank
    character string is returned.                                 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "vmachdep.h"
#include <string.h>

/*    Fortran Callable Subroutine    */

void zgtprcs(char s[8]);

char FTN_NAME2(gtprcs, GTPRCS) (char s[8], ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     char c_string[8];
     zgtprcs(c_string);
     zsc2for(c_string,8,s,&s,1,1,1, s);
}

/*    C-Callable Subroutine     */

void zgtprcs(s)
char s[8];
{
     char *cuserid_p2();

     {
          strcpy(s,cuserid_p2());
     }

}
