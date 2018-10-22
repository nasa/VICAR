/**
 * cuserid_p2() - simple wrapper to make cuserid() work on all platforms.
 * Needed because some don't implement (or have deprecated) cuserid.
 *
 * Returns the userid of the current process as a string.  The string must
 * be copied or used before calling again - this is NOT reentrant.
 */

#include "xvmaininc.h"
#include <stdio.h>

#if CUSERID_AVAIL_OS == 0
#include <pwd.h>
#endif

char *cuserid_p2()
{
#if CUSERID_AVAIL_OS
    char *cuserid();
    return cuserid(NULL);
#else
    struct passwd *pw;

    pw = getpwuid(getuid());
    return pw->pw_name;
#endif

}
