/************************************************************************/
/* Main include file for the VICAR Run-Time Library			*/
/************************************************************************/

#ifndef _XVMAININC_H
#define _XVMAININC_H

/* The following is modified from Xm.h.  If _NO_PROTO is defined, we	*/
/* don't want to use prototypes.  _NO_PROTO could be defined on the	*/
/* compile line if it's really needed.					*/

#ifndef _NO_PROTO
#if !defined(__STDC__) && !defined(__cplusplus) && !defined(FUNCPROTO)
#define _NO_PROTO
#endif /* __STDC__ */
#endif /* _NO_PROTO */

/* The following can be used to silence warnings about unused		*/
/* arguments, if desired.  Obviously this is compiler-specific.		*/

#ifndef UNUSED
#if defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif
#endif

/* The following two #defines select which operating system to compile	*/
/* for.  They are automatically set by predefined preprocessor symbols.	*/

#if defined(vms) || defined(__VMS)
#define UNIX_OS	0
#define VMS_OS	1
#else
#define UNIX_OS	1
#define VMS_OS	0
#endif

/* The following #defines select the architecture of the system you are	*/
/* on, i.e. a subdivision of the UNIX_OS and VMS_OS defines.  These are	*/
/* used almost exclusively to set the various feature #defines at the	*/
/* bottom of this file.  Do not use a _ARCH directly in an #if in	*/
/* program code.  Rather, abstract the thing you want into a feature	*/
/* #define, set below, that is based on the architecture.  This makes	*/
/* porting to new machines much easier.  The _ARCH symbols are set	*/
/* automatically by predefined preprocessor symbols.			*/

#define VAX_ARCH	0
#define ALPHA_ARCH	0
#define SUN3_ARCH	0
#define SUN4_ARCH	0
#define SUN_SOLARIS_ARCH 0
#define X86_SOLARIS_ARCH 0
#define ALLIANT_ARCH	0
#define DECSTATION_ARCH	0
#define CRAY_ARCH	0
#define MAC_AUX_ARCH	0
#define MAC_MPW_ARCH	0
#define SGI_ARCH	0
#define TEK_ARCH	0
#define HP700_ARCH	0
#define AXP_UNIX_ARCH	0
#define AXP_LINUX_ARCH	0	/* Blame Myche McAuley for this Linux	*/
#define X86_LINUX_ARCH	0	/* stuff.  Hey...I tried		*/
#define X86_64_LINX_ARCH 0
#define PPC_LINUX_ARCH	0
#define MAC_OSX_ARCH	0
#define X86_MACOSX_ARCH	0
#define MAC64_OSX_ARCH	0
#define X86_NT_ARCH	0	/* Simon Hook */

#if VMS_OS
#ifdef __ALPHA
#undef	ALPHA_ARCH
#define	ALPHA_ARCH	1
#else
#undef	VAX_ARCH
#define	VAX_ARCH	1
#endif
#endif

#ifdef ultrix
#undef	DECSTATION_ARCH
#define	DECSTATION_ARCH	1
#endif

/* The V2_FORCE_32 test is needed only in vimake.  Once we add -m32 to	*/
/* the compile lines, __x86_64__ is automatically undefined.		*/
#if defined (__linux__) || defined (linux)
#if defined (__alpha__) || defined (alpha)
#undef	AXP_LINUX_ARCH
#define	AXP_LINUX_ARCH	1
#else
#if defined (__PPC__) || defined (powerpc)
#undef	PPC_LINUX_ARCH
#define	PPC_LINUX_ARCH  1
#else
#if defined (__i386__) || defined (i386) || defined(V2_FORCE_32)
#undef	X86_LINUX_ARCH
#define	X86_LINUX_ARCH	1
#else
#if defined (__x86_64__)
#undef	X86_64_LINX_ARCH
#define	X86_64_LINX_ARCH 1
#endif
#endif
#endif
#endif
#endif

#if UNIX_OS && defined(__alpha) && AXP_LINUX_ARCH==0
#undef	AXP_UNIX_ARCH
#define	AXP_UNIX_ARCH	1
#endif

/* The V2_FORCE_32 test is needed only in vimake.  Once we add -m32 to  */
/* the compile lines, __x86_64__ is automatically undefined.            */
#if defined(__APPLE__)
#if defined(__ppc__)
#undef	MAC_OSX_ARCH
#define	MAC_OSX_ARCH	1
#else
#if defined(__x86_64__) && !defined(V2_FORCE_32)
#undef  MAC64_OSX_ARCH
#define MAC64_OSX_ARCH 1
#else
#undef	X86_MACOSX_ARCH
#define	X86_MACOSX_ARCH	1
#endif
#endif
#endif

#ifdef sun
#ifdef sparc
#ifdef SOLARIS	/* -DSOLARIS : this is not predefined by the compiler! (grr) */
#undef  SUN_SOLARIS_ARCH
#define SUN_SOLARIS_ARCH 1
#else
#undef	SUN4_ARCH
#define	SUN4_ARCH	1
#endif
#else
#ifdef i386
#undef  X86_SOLARIS_ARCH
#define X86_SOLARIS_ARCH 1
#else
#undef	SUN3_ARCH
#define	SUN3_ARCH	1
#endif
#endif
#endif

#ifdef alliant
#undef	ALLIANT_ARCH
#define	ALLIANT_ARCH	1
#endif

#ifdef CRAY
#undef	CRAY_ARCH
#define	CRAY_ARCH	1
#endif

#ifdef _AUX_SOURCE
#undef	MAC_AUX_ARCH
#define	MAC_AUX_ARCH	1
#endif

#ifdef applec
#undef	MAC_MPW_ARCH
#define MAC_MPW_ARCH	1
#endif

#if defined(__sgi) || defined(sgi)
#undef	SGI_ARCH
#define	SGI_ARCH	1
#endif

#if defined(m88k) && defined(ghs)	/* need better conditions! */
#undef	TEK_ARCH
#define	TEK_ARCH	1
#endif

#ifdef WIN32
#undef	X86_NT_ARCH
#define	X86_NT_ARCH	1
#endif

/* On the 700 series, the 700 symbol is defined in the _cc_ command,	*/
/* *NOT* the _cpp_ command!!!  This means imake won't find the proper	*/
/* predefined symbol.  So, for now, use the 800 series symbol (which is	*/
/* defined in cpp on the 700) as well.					*/
#if defined(__hp9000s700) || defined(__hp9000s800)	/* Fix this!! */
#undef HP700_ARCH
#define HP700_ARCH	1
#endif

/* Error check - Let's make sure that _a single_ OS was selected*/

#if VAX_ARCH+ALPHA_ARCH+SUN3_ARCH+SUN4_ARCH+SUN_SOLARIS_ARCH+X86_SOLARIS_ARCH+ALLIANT_ARCH+DECSTATION_ARCH+CRAY_ARCH+MAC_AUX_ARCH+MAC_MPW_ARCH+SGI_ARCH+TEK_ARCH+HP700_ARCH+AXP_UNIX_ARCH+AXP_LINUX_ARCH+X86_LINUX_ARCH+X86_64_LINX_ARCH+PPC_LINUX_ARCH+MAC_OSX_ARCH+X86_MACOSX_ARCH+MAC64_OSX_ARCH+X86_NT_ARCH != 1
Compile Error: *_ARCH either not defined or not defined correctly!!!!!!!!!!!
#endif

/* Shortcut for Linuxes */
#if AXP_LINUX_ARCH + X86_LINUX_ARCH + X86_64_LINX_ARCH + PPC_LINUX_ARCH
#define ANY_LINUX_ARCH 1
#else
#define ANY_LINUX_ARCH 0
#endif

/* Shortcut for Mac OS X */
#if MAC_OSX_ARCH + X86_MACOSX_ARCH + MAC64_OSX_ARCH
#define ANY_OSX_ARCH 1
#else
#define ANY_OSX_ARCH 0
#endif

/* The following #defines select optional pieces of the RTL.  1 means	*/
/* use the piece, 0 means do not use it.				*/
/* The TAE major version (4 or 5) should be set based on what is	*/
/* available on your platform.  It is needed only by builds (imake etc).*/
/* TAPE has only been tested under VMS and on a Sun-4, so turn it off	*/
/* automatically on other platforms.  It may work on other platforms;	*/
/* if you need it, try it.						*/

#define RTL_USE_TAE	1	/* use TAE subroutine package */
#define TAE_VERSION	5

#if SUN4_ARCH+VAX_ARCH+ALPHA_ARCH
#define RTL_USE_TAPE	1	/* include VICAR-format tape support */
#else
#define RTL_USE_TAPE	0	/* don't include VICAR-format tape support */
#endif

/* RTL_USE_SHELL_VIC is my addition for implementing shell vicar code	*/
/*	BUT you still need to set RTL_USE_TAE (above) to 1		*/
#define RTL_USE_SHELL_VIC	1	/* Use shell vicar routines */

/* Turn this on to enable support for compression.  It doesn't control	*/
/* any specific compression mechanism; it rather turns on or off the	*/
/* infrastructure to even look for compression.				*/
#define RTL_USE_COMPRESSION	0

/************************************************************************/
/* The following macros do NOT need to be redefined when compiling on a	*/
/* different system.  Changing the macros above will control what is	*/
/* selected below.							*/
/************************************************************************/

/************************************************************************/
/* Data-type defines.  The following set up the defaults for the data	*/
/* type system label items.						*/
/* NOTE:  If you change or add an entry, make sure you update the table	*/
/* in xvhost.c!								*/
/************************************************************************/

#if VAX_ARCH
#define NATIVE_HOST_LABEL	"VAX-VMS"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"VAX"
#endif

#if ALPHA_ARCH
#define NATIVE_HOST_LABEL	"AXP-VMS"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"VAX"	/* Would like IEEE, but can't use it */
#endif

#if SUN3_ARCH
#define NATIVE_HOST_LABEL	"SUN-3"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if SUN4_ARCH
#define NATIVE_HOST_LABEL	"SUN-4"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if SUN_SOLARIS_ARCH
#define NATIVE_HOST_LABEL       "SUN-SOLR"
#define NATIVE_INTFMT           "HIGH"
#define NATIVE_REALFMT          "IEEE"
#endif

#if X86_SOLARIS_ARCH
#define NATIVE_HOST_LABEL	"X86-SOLR"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if ALLIANT_ARCH
#define NATIVE_HOST_LABEL	"ALLIANT"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if DECSTATION_ARCH
#define NATIVE_HOST_LABEL	"DECSTATN"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if CRAY_ARCH
#define NATIVE_HOST_LABEL	"CRAY"
#define NATIVE_INTFMT		"???"
#define NATIVE_REALFMT		"???"
#endif

#if MAC_AUX_ARCH
#define NATIVE_HOST_LABEL	"MAC-AUX"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if MAC_MPW_ARCH
#define NATIVE_HOST_LABEL	"MAC-MPW"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if SGI_ARCH
#define NATIVE_HOST_LABEL	"SGI"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if TEK_ARCH
#define NATIVE_HOST_LABEL	"TEK"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if HP700_ARCH
#define NATIVE_HOST_LABEL	"HP-700"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if AXP_UNIX_ARCH
#define NATIVE_HOST_LABEL	"AXP-UNIX"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if AXP_LINUX_ARCH
#define NATIVE_HOST_LABEL	"AXP-LINUX"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if X86_LINUX_ARCH
#define NATIVE_HOST_LABEL	"X86-LINUX"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if X86_64_LINX_ARCH
#define NATIVE_HOST_LABEL	"X86-64-LINX"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if PPC_LINUX_ARCH
#define NATIVE_HOST_LABEL	"PPC-LINUX"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if MAC_OSX_ARCH
#define NATIVE_HOST_LABEL	"MAC-OSX"
#define NATIVE_INTFMT		"HIGH"
#define NATIVE_REALFMT		"IEEE"
#endif

#if X86_MACOSX_ARCH
#define NATIVE_HOST_LABEL	"X86-MACOSX"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if MAC64_OSX_ARCH
#define NATIVE_HOST_LABEL	"MAC64-OSX"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

#if X86_NT_ARCH
#define NATIVE_HOST_LABEL	"X86-NT"
#define NATIVE_INTFMT		"LOW"
#define NATIVE_REALFMT		"RIEEE"
#endif

/************************************************************************/
/* OS-related defines							*/
/************************************************************************/

/* Set up mechanisms for handling global variables.  The difference is	*/
/* needed because under VMS, all "globaldef" variables go in a single	*/
/* psect, while all "extern" variables go in individual psects.  Since	*/
/* the data psects must be included in the linker options file in order	*/
/* for the shareable image to be linked, it is easier to deal with one	*/
/* psect than dozens of them.						*/

#if UNIX_OS

#define PUBLICDEF
#define PUBLICREF extern

#endif /* UNIX_OS */

#if VMS_OS

#define PUBLICDEF globaldef
#define PUBLICREF globalref

#endif /* VMS_OS */

/************************************************************************/
/* Architechture-related defines					*/
/************************************************************************/

/* If the actual number of arguments are available, set NARGS_AVAIL_OS	*/
/* to 1 and define the macro "va_count(nargs)" to set the integer nargs.*/
/* The varargs environment is assumed to be set up already.		*/

#if VAX_ARCH + ALPHA_ARCH
#define NARGS_AVAIL_OS 1
/* va_count is system-defined */
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + DECSTATION_ARCH + CRAY_ARCH + SGI_ARCH + TEK_ARCH + ANY_LINUX_ARCH
#define NARGS_AVAIL_OS 0
#endif

#if MAC_AUX_ARCH + MAC_MPW_ARCH + HP700_ARCH + AXP_UNIX_ARCH + ANY_OSX_ARCH + X86_NT_ARCH
#define NARGS_AVAIL_OS 0
#endif

#if ALLIANT_ARCH
#define NARGS_AVAIL_OS 1
#define va_count(nargs) nargs = *(((short int *)(&va_alist))-1)
#endif

/************************************************************************/
/* If fstat() returns the optimal blocksize, set FSTAT_BLKSIZE_OS to 1.	*/
/* If not, set it to 0.							*/

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + DECSTATION_ARCH + ALLIANT_ARCH + MAC_AUX_ARCH + AXP_UNIX_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH
#define FSTAT_BLKSIZE_OS 1
#endif

#if VAX_ARCH + ALPHA_ARCH + CRAY_ARCH + MAC_MPW_ARCH + SGI_ARCH + TEK_ARCH + HP700_ARCH + X86_NT_ARCH
#define FSTAT_BLKSIZE_OS 0
#endif

/************************************************************************/
/* If fstat() is even available, set FSTAT_AVAIL_OS to 1.  If not,	*/
/* set it to 0.  Note that if fstat() is not available, FSTAT_BLKSIZE_OS*/
/* should also be set to 0.						*/

#if VAX_ARCH + ALPHA_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + DECSTATION_ARCH + ALLIANT_ARCH + CRAY_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if SGI_ARCH + TEK_ARCH + HP700_ARCH + AXP_UNIX_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH + X86_NT_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if MAC_AUX_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if MAC_MPW_ARCH
#define FSTAT_AVAIL_OS 0
#endif

/************************************************************************/
/* If mmap() is available (for array I/O), then set MMAP_AVAIL_OS to 1.	*/
/* If not, set it to 0.							*/

#if VAX_ARCH + ALPHA_ARCH
#define MMAP_AVAIL_OS	0		/* doesn't matter for VMS */
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + SGI_ARCH
#define MMAP_AVAIL_OS	1
#endif

#if CRAY_ARCH
#define MMAP_AVAIL_OS	???		/* check on this */
#endif

#if ALLIANT_ARCH + MAC_MPW_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH + TEK_ARCH + X86_NT_ARCH
#define MMAP_AVAIL_OS	0
#endif

#if HP700_ARCH + AXP_UNIX_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH
#define MMAP_AVAIL_OS	1
#endif

/************************************************************************/
/* If the on_exit() routine is available (sets up an exit handler),	*/
/* then set ON_EXIT_AVAIL_OS to 1.  If not, set it to 0.		*/

#if VAX_ARCH + ALPHA_ARCH
#define ON_EXIT_AVAIL_OS	0	/* doesn't matter under VMS */
#endif

#if SUN3_ARCH + SUN4_ARCH + ANY_LINUX_ARCH
#define ON_EXIT_AVAIL_OS	1
#endif

#if SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + DECSTATION_ARCH + ALLIANT_ARCH + MAC_AUX_ARCH + SGI_ARCH + TEK_ARCH + ANY_OSX_ARCH
#define ON_EXIT_AVAIL_OS	0
#endif

#if HP700_ARCH + AXP_UNIX_ARCH
#define ON_EXIT_AVAIL_OS	0
#endif

#if CRAY_ARCH
#define ON_EXIT_AVAIL_OS	???		/* check on this */
#endif

#if MAC_MPW_ARCH	/* actually exists as atexit() if needed */
#define ON_EXIT_AVAIL_OS	0		/*????*/
#endif

#if X86_NT_ARCH		/* actually exists as _onexit() if needed */
#deifne ON_EXIT_AVAIL_OS	0
#endif

/************************************************************************/
/* Since there is little standardization on the definitions for the	*/
/* lseek() call, set up the include to use and define SEEK_SET and	*/
/* SEEK_END if they aren't already.					*/
/* Note:  If you include seek_include, you must include <sys/types.h>	*/
/* first for MAC_AUX_ARCH!						*/

#if VAX_ARCH + ALPHA_ARCH
#define seek_include	<stdio.h>
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + DECSTATION_ARCH + SGI_ARCH + TEK_ARCH + HP700_ARCH + AXP_UNIX_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH
#define seek_include	<unistd.h>
#endif

#if ALLIANT_ARCH
#define seek_include	<sys/file.h>
#define SEEK_SET	L_SET
#define SEEK_END	L_XTND
#endif

#if CRAY_ARCH
#define seek_include	????
#endif

#if MAC_AUX_ARCH + X86_NT_ARCH
#define seek_include	<stdio.h>
#endif

#if MAC_MPW_ARCH
#define seek_include	<fcntl.h>
#endif

/************************************************************************/
/* Some implementations of open() don't allow the Unix-style protection	*/
/* flags as the third argument.						*/

#if VAX_ARCH + ALPHA_ARCH
#define OPEN_PROTECT_OS	1
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + ALLIANT_ARCH + DECSTATION_ARCH + SGI_ARCH + TEK_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH
#define OPEN_PROTECT_OS	1
#endif

#if CRAY_ARCH + MAC_AUX_ARCH + HP700_ARCH + AXP_UNIX_ARCH + X86_NT_ARCH
#define OPEN_PROTECT_OS	1
#endif

#if MAC_MPW_ARCH
#define OPEN_PROTECT_OS	0
#endif

/************************************************************************/
/* Set FTRUNCATE_AVAIL_OS if the ftruncate() function is available.	*/
/* Note that it does not hurt if ftruncate() isn't there, files just	*/
/* won't be pre-allocated or truncated on close.			*/

#if VAX_ARCH + ALPHA_ARCH
#define FTRUNCATE_AVAIL_OS	0
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH + SGI_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH
#define FTRUNCATE_AVAIL_OS	1
#endif

#if HP700_ARCH + AXP_UNIX_ARCH
#define FTRUNCATE_AVAIL_OS	1
#endif

#if ALLIANT_ARCH + MAC_MPW_ARCH + TEK_ARCH + X86_NT_ARCH
#define FTRUNCATE_AVAIL_OS	0
#endif

#if CRAY_ARCH
#define FTRUNCATE_AVAIL_OS	???
#endif

/************************************************************************/
/* Set HOSTMSG_UNIX_OS if the host error messages can be obtained via	*/
/* sys_nerr and sys_errlist[].  If not, set HOSTMSG_UNIX_OS to 0 and	*/
/* define HOSTMSG_FUNC_OS to be a function that returns a pointer to	*/
/* the error string given the error number as an argument.		*/

#if VAX_ARCH + ALPHA_ARCH
#define HOSTMSG_UNIX_OS	0	/* HOSTMSG_FUNC_OS doesn't matter for VMS */
#endif

#if SUN3_ARCH + SUN4_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + ALLIANT_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH
#define HOSTMSG_UNIX_OS	1
#endif

#if SGI_ARCH + TEK_ARCH + HP700_ARCH + AXP_UNIX_ARCH
#define HOSTMSG_UNIX_OS	1
#endif

#if MAC_MPW_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH + X86_NT_ARCH
#define HOSTMSG_UNIX_OS	0
#define HOSTMSG_FUNC_OS	strerror
#endif

#if CRAY_ARCH
#define HOSTMSG_UNIX_OS	???
#endif

/************************************************************************/
/* If getdtablesize() is available, set GETDTABLESIZE_OS to 1.		*/
/* Otherwise, set it to 0, in which case RLIMIT_NOFILE in getrlimit()	*/
/* must be available.  (this is used in the VRDI)			*/

#if VAX_ARCH + ALPHA_ARCH
#define GETDTABLESIZE_OS 1
#endif

#if SUN3_ARCH + SUN4_ARCH + SGI_ARCH + MAC_AUX_ARCH + AXP_UNIX_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH
#define GETDTABLESIZE_OS 1
#endif

#if HP700_ARCH + SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + X86_NT_ARCH
#define GETDTABLESIZE_OS 0
#endif

#if ALLIANT_ARCH + DECSTATION_ARCH + TEK_ARCH + MAC_MPW_ARCH + CRAY_ARCH
#define GETDTABLESIZE_OS ???
#endif

/************************************************************************/
/* If sigvec() is available, define SIGVEC_OS to 1.  Otherwise, define	*/
/* it to 0, in which case sigvector() must be available and do the same	*/
/* thing.  If the operating system is SUN_SOLARIS_ARCH, define SIGACT_OS*/
/* to be 1, because sigaction() is available. (this is used in the VRDI)*/

#if VAX_ARCH + ALPHA_ARCH
#define SIGACT_OS 0
#define SIGVEC_OS 1
#endif

#if SUN3_ARCH + SUN4_ARCH + SGI_ARCH + MAC_AUX_ARCH + AXP_UNIX_ARCH + ANY_LINUX_ARCH + ANY_OSX_ARCH
#define SIGACT_OS 0
#define SIGVEC_OS 1
#endif

#if HP700_ARCH
#define SIGACT_OS 0
#define SIGVEC_OS 0
#endif

#if SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH
#define SIGACT_OS 1
#define SIGVEC_OS 0
#endif

#if ALLIANT_ARCH + DECSTATION_ARCH + TEK_ARCH + MAC_MPW_ARCH + CRAY_ARCH + X86_NT_ARCH
#define SIGACT_OS ???
#define SIGVEC_OS ???
#endif

/**************************************************************************/
/* For Solaris, the rindex() function is non-existent.  For this case	  */
/* and this case only, the function strchr() will replace rindex()	  */
/* and therefore STRCHR_OS must be set to one.                            */

#if SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH + X86_NT_ARCH
#define STRCHR_AVAIL_OS 1
#else
#define STRCHR_AVAIL_OS 0
#endif

/**************************************************************************/
/* For Solaris, the <sys/file.h> include file does not work because	  */
/* Solaris is not BSD Unix.  If FCNTL_AVAIL_OS is set, <fcntl.h>	  */
/* should be used instead.						  */

#if SUN_SOLARIS_ARCH + X86_SOLARIS_ARCH
#define FCNTL_AVAIL_OS 1
#else
#define FCNTL_AVAIL_OS 0
#endif

/**************************************************************************/
/* If cuserid() is available, set this to 1.  Otherwise, we use an	  */
/* alternate:  pw=getpwuid(getuid()), pw->pw_name.			  */
/* Note that for historical reasons, MAC_MPW and ALLIANT do things	  */
/* slightly differently, and define a cuserid() function in special	  */
/* source files.  I just didn't feel like changing these.		  */

#if ANY_OSX_ARCH
#define CUSERID_AVAIL_OS	0
#else
#define CUSERID_AVAIL_OS	1
#endif

/**************************************************************************/
/* If lockf() is available, set this to 1.  Otherwise, we must use	  */
/* flock() instead (which requires <sys/file.h>, rather than <unistd.h>). */

#if ANY_OSX_ARCH
#define LOCKF_AVAIL_OS		0
#else
#define LOCKF_AVAIL_OS		1
#endif

/**************************************************************************/
/* If the Sybase libraries are available, define this symbol to 1.        */
/* Otherwise, define it to 0.  External sites may need to define this to  */
/* 0 for all platforms (used by e.g. galsos).				  */
/* Added X86_LINUX_ARCH after port of galsos to LINUX -jaw                */

#if SUN_SOLARIS_ARCH + SUN4_ARCH + HP700_ARCH + ALPHA_ARCH + VAX_ARCH + SGI_ARCH + X86_LINUX_ARCH
#define SYBASE_AVAIL_OS 1
#else
#define SYBASE_AVAIL_OS 0
#endif

/**************************************************************************/
/* If the Mysql libraries are available, define this symbol to 1.         */
/* Otherwise, define it to 0.  External sites may need to define this to  */
/* 0 for all platforms.                                                   */
/* Only X86_LINUX_ARCH supported for now.                                 */

#if X86_LINUX_ARCH
#define MYSQL_AVAIL_OS 1
#else
#define MYSQL_AVAIL_OS 0
#endif

/**************************************************************************/
/* If pointers are 64 bits, set this flag to 1.				  */

#if AXP_UNIX_ARCH + AXP_LINUX_ARCH + ANY_OSX_ARCH
#define POINTERS_64_BITS_OS 1
#else
#if defined(__x86_64__)
#define POINTERS_64_BITS_OS 1
#else
#define POINTERS_64_BITS_OS 0
#endif
#endif

/**************************************************************************/
/* If Large Files ( > 2GB) are supported, set this flag to 1.  Note that  */
/* _LARGEFILE64_SOURCE is a system symbol on most machines which support  */
/* this, and it is possible that it might be turned on by default,        */
/* without being turned on explicitly here.  Also, some machines (e.g.	  */
/* AXP-UNIX) support large files natively, with standard open(), lseek()  */
/* etc. calls.  These machines need no additional support since V2_OFFSET */
/* will be defined properly (as off_t).					  */
/*									  */
/* The type "V2_OFFSET" is defined to be either off64_t, or off_t,	  */
/* depending on the setting of this flag.  We use this rather than a	  */
/* system-defined type because it's less confusing this way and works	  */
/* in either largefile or smallfile environments.			  */
/*									  */
/* For similar reasons, we define V2_xxx calls here, which set up either  */
/* the standard (e.g. open, lseek) or large (e.g. open64, lseek64)	  */
/* versions of these functions.  This way, there's only one place to go	  */
/* to change things.  Applications may use these too, if desired.	  */
/* This is not a complete list of xxx64 functions, merely the ones	  */
/* currently in use.							  */
/*									  */
/* In order to support Solaris 2.5.1, we include sys/feature_tests.h and  */
/* check to see if _FILE_OFFSET_BITS is defined.  If not, we must be	  */
/* in 2.5.1 or earlier, so we turn large file support back off (since it  */
/* didn't exist yet).							  */

#if SUN_SOLARIS_ARCH
#define _LARGEFILE64_SOURCE 1		/* try it... */
#include <sys/feature_tests.h>
#ifndef _FILE_OFFSET_BITS
#undef _LARGEFILE64_SOURCE		/* not there after all */
#endif
#endif

#if X86_LINUX_ARCH + PPC_LINUX_ARCH
#define _FILE_OFFSET_BITS 64
#define _LARGEFILE64_SOURCE 1
#endif

#if SGI_ARCH && !defined(__STDC__)
#define _LARGEFILE64_SOURCE 1
#endif
#if HP700_ARCH
#define _LARGEFILE64_SOURCE 1
#endif

#ifdef _LARGEFILE64_SOURCE
#define __USE_LARGEFILE64 1
/* types, structures */
#define V2_OFFSET off64_t
#define V2_STATBUF stat64
/* functions */
#define V2_OPEN open64
#define V2_FSTAT fstat64
#define V2_LSEEK lseek64
#define V2_FTRUNCATE ftruncate64
#define V2_FOPEN fopen64
#define V2_FTELLO ftello64
#define V2_FSEEKO fseeko64
#else
/* types, structures */
#if VMS_OS
#define V2_OFFSET int
#else
#define V2_OFFSET off_t
#endif
#define V2_STATBUF stat
/* functions */
#define V2_OPEN open
#define V2_FSTAT fstat
#define V2_LSEEK lseek
#define V2_FTRUNCATE ftruncate
#define V2_FOPEN fopen
#define V2_FTELLO ftell
#define V2_FSEEKO fseek
#endif

#endif /* _XVMAININC_H */

