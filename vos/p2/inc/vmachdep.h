/******************************************************************************/
/* Include file for defining machine dependencies beyond those in xvmaininc.h */
/******************************************************************************/

#if !defined VMACHDEP_H_INCLUDED
#define VMACHDEP_H_INCLUDED

#include "xvmaininc.h"

/************************************************************************/
/* OS-related defines							*/
/************************************************************************/


/************************************************************************/
/* If select() is available (for timing and checking file descriptors),
   then set SELECT_AVAIL_OS to 1.   If not, set it to 0.		*/

#if VAX_ARCH + ALPHA_ARCH
#define SELECT_AVAIL_OS	0		/* doesn't matter for VMS */
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + SGI_ARCH
#define SELECT_AVAIL_OS	1
#endif

#if ALLIANT_ARCH + HP700_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH + TEK_ARCH
#define SELECT_AVAIL_OS	1
#endif

#if CRAY_ARCH
#define SELECT_AVAIL_OS	???		/* check on this */
#endif

#if MAC_MPW_ARCH 
#define SELECT_AVAIL_OS	0
#endif

#if ANY_LINUX_ARCH + ANY_OSX_ARCH
#define SELECT_AVAIL_OS 1 
#endif

/************************************************************************/
/*  IF THE CUSERID() FUNCTION CAN BE USED FOR THE MACHINE 
    ARCHITECTURE THEN CUSERID_AVAIL_OS IS GIVEN THE VALUE OF 1 */

#if MAC_MPW_ARCH
#define CUSERID_AVAIL_OS  0
#endif

#if VAX_ARCH + ALPHA_ARCH
#define CUSERID_AVAIL_OS  1
#endif

#if CRAY_ARCH
#define CUSERID_AVAIL_OS ??? 
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + SGI_ARCH
#define CUSERID_AVAIL_OS  1
#endif

#if ALLIANT_ARCH 
#define CUSERID_AVAIL_OS  0
#endif

#if HP700_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH + TEK_ARCH
#define CUSERID_AVAIL_OS  1
#endif

#if ANY_LINUX_ARCH
#define CUSERID_AVAIL_OS  1
#endif

#if ANY_OSX_ARCH
#define CUSERID_AVAIL_OS  0
#endif

/***************************************************************************/
/* On the Alpha, Sybase compiles its libraries with IEEE_float rather than */
/* d_float.  For as long as our software must be compatible with the VAX   */
/* d_float type, we need to convert the Sybase IEEEfloats to dfloat. (This */
/* conversion should take place only on the Alpha; everywhere else we      */
/* compile with ieee_float.)                                               */
/***************************************************************************/
#if ALPHA_ARCH
#define CONVERT_SYBASE_IEEE2D 1
#else
#define CONVERT_SYBASE_IEEE2D 0
#endif


/**************************************************************************/
/* In <sys/socket.h>, parameter 3 of 'accept' is type int*                */
/* for most compilers, but is type socklen_t* (which is an unsigned int)  */
/* for g++.  Earlier versions of g++ just issued a warning, but           */
/* g++ 2.95.2 says it's an error for parameter 3 to be a int*.            */
/*                                                                        */
/* Defining parameter 3 as a SOCKLEN_T* will work for all platforms.      */
/*                                                                        */
/* Note that this may just be a bug in g++.  When g++ has been upgraded   */
/* past 2.95.2, this fix may no longer be neccessary.                     */
/**************************************************************************/

#if ANY_LINUX_ARCH
#define V2_SOCKLEN_T socklen_t
#else
#define V2_SOCKLEN_T int
#endif


#endif /* !defined VMACHDEP_H_INCLUDED */


