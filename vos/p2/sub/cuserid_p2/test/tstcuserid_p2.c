/**
 * Routine to test cuserid_p2.  All it does it print the current user - no
 * other test is possible or useful.
 */
#include "vicmain_c"
#include <stdio.h>

main44()
{
    printf("current user is: '%s'\n", cuserid_p2());
}

