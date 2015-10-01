/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/

#ifndef TAE
#include "xvmaininc.h"
#else
#define VMS_OS 1		/* TAE only used for VMS */
#define UNIX_OS 0
#endif

#ifndef TAE
#ifndef VRDI
#define VRDI		/* default */
#endif
#endif

/*	Allocation/deallocation include file				*/

#if VMS_OS
#include <stdio.h>
#include <descrip.h>
#include <ssdef.h>
#include <ctype.h>
#include <jpidef.h>
#include <dvidef.h>
#include <psldef.h>
#include <secdef.h>
#include <rms.h>
#endif /* VMS_OS */

#if UNIX_OS
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#endif /* UNIX_OS */

#if VMS_OS
#define VAX_SYMBOL_STRING		"MIPLVAX"
#ifndef	JPI$_MASTER_PID
#define	JPI$_MASTER_PID	0x00000325	/* Because "jpidef" does not	*/
#endif	/* JPI$_MASTER_PID */

#ifndef	LIB$_NOSUCHSYM
#define	LIB$_NOSUCHSYM	0x00158364	/* Because there is no .h file	*/
#endif	/* LIB$_NOSUCHSYM */

#ifndef	LNM$_STRING
#define	LNM$_STRING	0x00000002	/*	...ditto...		*/
#endif	/* LNM$_STRING */

#ifndef	DVI$_TT_PHYDEVNAM
#define	DVI$_TT_PHYDEVNAM	274	/* Because "dvidef" does not	*/
#endif	/* DVI$_TT_PHYDEVNAM */

/*	Define system service routines					*/

extern	int	LIB$GET_SYMBOL(),SYS$GETJPIW(), SYS$GETDVIW();
extern	int	SYS$CMEXEC(), SYS$ALLOC(), SYS$DALLOC();
extern	int	SYS$CRELNM(), SYS$DELLNM();

#endif /* VMS_OS */

/*	Macros to determine if two strings "match"			*/

#ifndef MAX
#define	MAX( n, m )	(((n) > (m)) ? (n) : (m))
#endif  /* MAX */

#define	MATCH( str1, str2, n )	\
			(0 == strncmp( str1, str2, MAX(n,strlen(str1))))

/*	Macro to check for "bad" system errors				*/

#if VMS_OS
#define	BadError	(SystemError != SS$_NORMAL)
#endif /* VMS_OS */

#if UNIX_OS
#define NO_ERROR 0
#define	BadError	(SystemError != NO_ERROR)
#endif /* UNIX_OS */

/*	Define string sizes for various device information strings	*/

#define	USER_NAME_SIZE		15
#define	TERMINAL_NAME_SIZE	10
#define	USER_TERMINAL_SIZE	10

#define	LOW_RESOLUTION		'L'
#define	HIGH_RESOLUTION		'H'
#define	BOTH_RESOLUTIONS	'B'

/*	Device information structure					*/

PRIVATE struct	DEVICE_INFO {
#ifdef	VRDI
   char	UserName[USER_NAME_SIZE+1];		/* User name that owns	*/
   int	UserPID;				/* User PID that owns	*/
   char	UserTerminal[TERMINAL_NAME_SIZE+1];	/* Terminal owner is at	*/
#endif	/* VRDI */
#ifdef	TAE
   int	UserPID;				/* User PID that owns	*/
#endif	/* TAE */
   }	Devices[MAXIMUM_UNITS];

#define	DEVICE	Devices[DeviceNumber]	/* A Macro to make life easy	*/

#if VMS_OS

/*	Itemlist for SYS$TRNLNM						*/

PRIVATE struct {

   short Length;
   short ItemCode;
   int   *Address;
   int   *RetAddr;
   int   End;
} Itemlist;

/*	Arguement list used for calling SYS$CMEXEC			*/

PRIVATE struct	 ARG_LIST {
   int	Count;
   int	Arg1;
   int	Arg2;
   int	Arg3;
   int	Arg4;
   int	Arg5;
   int	Arg6;
   int	Arg7;
   int	Arg8;
   }	ArgListVrdi;

/*	A general structure for sending/receiving data to/from various
 *	system services
 */
PRIVATE struct {
      short	Length;
      short	ItemCode;
      int	*Address;
      int	*RetAddr;
      int	End;
      }	SysInformation;

#endif /* VMS_OS */

#ifdef	XD_INITIALIZE
#if VMS_OS
PRIVATE int SystemError = SS$_NORMAL;/* Latest sys service error code	*/
PRIVATE struct	dsc$descriptor_s	/* A utility string descriptor	*/
	StringDescriptor = {
	   0, 
	   DSC$K_DTYPE_T, 
	   DSC$K_CLASS_S, 
	   0
	   };
PRIVATE struct	dsc$descriptor_s	/* A utility string descriptor	*/
	LogicalNameTable = {
	   0, 
	   DSC$K_DTYPE_T, 
	   DSC$K_CLASS_S, 
	   0
	   };
#endif /* VMS_OS */

#if UNIX_OS
PRIVATE int SystemError = NO_ERROR;      /* Latest sys service error code */
#endif /* UNIX_OS */

#else	/* XD_INITIALIZE */

PRIVATE int SystemError;
#if VMS_OS
PRIVATE	struct	dsc$descriptor_s StringDescriptor;
PRIVATE struct	dsc$descriptor_s LogicalNameTable;
#endif /* VMS_OS */
#endif /* XD_INITIALIZE */

PRIVATE int ReturnLen;
PRIVATE int AccessMode;
PRIVATE int DeviceNumber;
PRIVATE int UserPID;		/* Process ID				*/
PRIVATE int UserVAX;		/* VAX Number				*/
PRIVATE char UserTerminal[USER_TERMINAL_SIZE+1];/* Terminal Name	*/
PRIVATE char Actual[10];
PRIVATE char LogicalUnit[10];
PRIVATE char LogicalTable[14];

/*	Contains the section name of the most recently allocated 	*/
/*	shared memory block.						*/

PRIVATE char SectionName[100];
