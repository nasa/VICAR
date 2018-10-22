/************************************************************************/
/* Definitions needed for FORTRAN bridge routines.			*/
/* This file must be included by ALL Fortran-callable C routines.	*/
/************************************************************************/

/************************************************************************/
/* The following macro makes a Fortran function name out of a C one.	*/
/* Most Unix machines want an '_' after the name in C if it's to be	*/
/* called by Fortran, but the Vax and some others do not need the '_'.	*/
/* Some architectures, such as the Cray, require upper case routine	*/
/* names if called by Fortran.  If this is the case, a global search	*/
/* and replace on FTN_NAME will be necessary to convert them to upper	*/
/* case - it cannot be done automatically.  (but see FTN_NAME2 below)	*/
/*									*/
/*	Again, you can blame McAuley for adding the linux stuff. 	*/
/************************************************************************/

#ifndef _FTNBRIDGE_H
#define _FTNBRIDGE_H

#include "xvmaininc.h"
#include <stdarg.h>

#if VAX_ARCH + ALPHA_ARCH + MAC_MPW_ARCH + HP700_ARCH + X86_NT_ARCH
#define FTN_NAME(a) a
#endif

#if SUN3_ARCH+SUN4_ARCH+SUN_SOLARIS_ARCH+ALLIANT_ARCH+DECSTATION_ARCH+MAC_AUX_ARCH+SGI_ARCH+AXP_UNIX_ARCH+ANY_LINUX_ARCH+ANY_OSX_ARCH
#ifdef __STDC__
#define FTN_NAME(a) a##_
#else
#define FTN_NAME(a) a/**/_
#endif
#endif

#if CRAY_ARCH
/* Needs to be upper case! */
#define FTN_NAME(a) a
#endif

/* Macros for more complete fortran naming support.  The F77_FUNC	*/
/* macros are part of autoconf; if they're not available (and they	*/
/* usually won't be, for standard VICAR) we use the above FTN_NAME	*/
/* macro.  The first argument should be lower case and the second	*/
/* upper, e.g. FTN_NAME2(name,NAME).  Use the trailing _ version if	*/
/* there are underscores in the name.  This is overkill, but will	*/
/* apparently handle most if not all Fortran compilers.  Currently, use	*/
/* of these "2" macros is optional (i.e. FTN_NAME() is still valid).	*/

#if defined(F77_FUNC) && defined(F77_FUNC_)
#define FTN_NAME2(a,AA) F77_FUNC(a,AA)
#define FTN_NAME2_(a,AA) F77_FUNC_(a,AA)
#else
#define FTN_NAME2(a,AA) FTN_NAME(a)
#define FTN_NAME2_(a,AA) FTN_NAME(a)
#endif

/************************************************************************/
/* The FORTRAN string passing method is defined below.  There are	*/
/* currently ten methods:						*/
/*									*/
/* STDARG) The new API and standard stdarg.h methods are used to	*/
/*    access the Fortran string lengths, which are contained in extra	*/
/*    parameters at the end as in method B.  This method is not		*/
/*    mutually exclusive with the others; it is possible to enable	*/
/*    stdarg for the new API while still allowing one of the other	*/
/*    methods for the old API.  Likewise, the new API can be used even	*/
/*    if STDARG is not enabled.						*/
/* A) The argument itself is a descriptor containing the string and	*/
/*    length (VMS)							*/
/* B) The argument is the string; the length is contained in extra	*/
/*    parameters at the end (Sun3, Sun4, Mac-A/UX, sgi-o32)		*/
/* C) The argument is the string; the length is in descriptors which	*/
/*    are before the normal argument list (Alliant)			*/
/* D) The argument itself is a descriptor, different format than	*/
/*    method A (Cray)							*/
/* E) The argument is the string; the length is in extra parameters	*/
/*    like method B, except every argument has space for a length, not	*/
/*    just strings.  Non-string lengths are undefined, however (mac-mpw)*/
/* F) The argument is the string; the length is in extra parameters	*/
/*    like method B, except the parameters go down in addresses instead	*/
/*    of up (hp-700)							*/
/* G) The argument is the string; the length is in extra parameters	*/
/*    like method B, except params are 64-bits.  Extra params must be	*/
/*    forced into memory via FTNSTRING_BLOCK (axp-unix, sgi-64)		*/
/* H) The argument is the string; the length is in extra parameters	*/
/*    like method B, except params are 64-bits.  However, if varargs is	*/
/*    not used, there is a 4-byte negative offset that must be applied.	*/
/*    Bizarre (sgi-n32)							*/
/* I) The argument is the string; the length is in extra parameters	*/
/*    like method B, except that we must dive into the stack frames,	*/
/*    since register and stack arguments are saved in different places.	*/
/*    See comments in sfor2c.c.  (ppc-linux).				*/
/*									*/
/* See the comments in sfor2c.c for the usage of the FORSTR_* and	*/
/* NEW_FORSTR_* macros.							*/
/************************************************************************/

#define ENABLE_NEW_API 1

#define FORSTR_METHOD_STDARG 0
#define FORSTR_METHOD_A 0
#define FORSTR_METHOD_B 0
#define FORSTR_METHOD_C 0
#define FORSTR_METHOD_D 0
#define FORSTR_METHOD_E 0
#define FORSTR_METHOD_F	0
#define FORSTR_METHOD_G	0
#define FORSTR_METHOD_H	0
#define FORSTR_METHOD_I	0

/* GFORTRAN (thus GCC) 7 defines the length as size_t rather than int */

#if defined(__GNUC__) && (__GNUC__ > 6)
#include <stddef.h>
#define FORSTR_STDARG_TYPE size_t
#else
#define FORSTR_STDARG_TYPE int
#endif

#if VAX_ARCH + ALPHA_ARCH
#undef FORSTR_METHOD_A
#define FORSTR_METHOD_A 1
#define FORSTR_PARAM _forstr_dummy
#define FORSTR_DEF int _forstr_dummy;
#define FORSTR_BLOCK
#endif

#if SUN3_ARCH + AXP_LINUX_ARCH + X86_LINUX_ARCH + X86_64_LINX_ARCH + X86_NT_ARCH
#undef FORSTR_METHOD_B
#define FORSTR_METHOD_B 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM _forstr_dummy
#define FORSTR_DEF int _forstr_dummy;
#define FORSTR_BLOCK
#endif

#if SUN4_ARCH+SUN_SOLARIS_ARCH+X86_SOLARIS_ARCH
#undef FORSTR_METHOD_B
#define FORSTR_METHOD_B 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM __builtin_va_alist
#define FORSTR_DEF int __builtin_va_alist;
#define FORSTR_BLOCK
#endif

#if ALLIANT_ARCH
#undef FORSTR_METHOD_C
#define FORSTR_METHOD_C 1
#undef ENABLE_NEW_API
#define ENABLE_NEW_API 0
#define FORSTR_PARAM _forstr_dummy
#define FORSTR_DEF int _forstr_dummy;
#define FORSTR_BLOCK
#endif

#if DECSTATION_ARCH
#undef FORSTR_METHOD_B
#define FORSTR_METHOD_B 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM va_alist
#define FORSTR_DEF int va_alist;
#define FORSTR_BLOCK
#endif

#if CRAY_ARCH
#undef FORSTR_METHOD_D
#define FORSTR_METHOD_D 1
#undef ENABLE_NEW_API
#define ENABLE_NEW_API 0
#define FORSTR_PARAM _forstr_dummy
#define FORSTR_DEF int _forstr_dummy;
#define FORSTR_BLOCK
#endif

#if MAC_AUX_ARCH
#undef FORSTR_METHOD_B
#define FORSTR_METHOD_B 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM _forstr_dummy
#define FORSTR_DEF int _forstr_dummy;
#define FORSTR_BLOCK
#endif

#if MAC_MPW_ARCH
#undef FORSTR_METHOD_E
#define FORSTR_METHOD_E 1
#undef ENABLE_NEW_API
#define ENABLE_NEW_API 0
#define FORSTR_PARAM _forstr_dummy
#define FORSTR_DEF int _forstr_dummy;
#define FORSTR_BLOCK
#endif

#if SGI_ARCH
/* SGI is weird; the method depends on the ABI in use.  -o32 is B,	*/
/* -n32 is H, and -64 is G.						*/
#include <sgidefs.h>
#if _MIPS_SIM == _MIPS_SIM_ABI32		/* -o32 */
#undef FORSTR_METHOD_B
#define FORSTR_METHOD_B 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM va_alist
#define FORSTR_DEF int va_alist;
#define FORSTR_BLOCK
#else
#if _MIPS_SIM == _MIPS_SIM_NABI32		/* -n32 */
#undef FORSTR_METHOD_H
#define FORSTR_METHOD_H 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM va_alist
#define FORSTR_DEF int va_alist;
#define FORSTR_BLOCK
#else						/* -64 */
#undef FORSTR_METHOD_G
#define FORSTR_METHOD_G 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM va_alist
#define FORSTR_DEF int va_alist;
#define FORSTR_BLOCK
#endif
#endif
#endif			/* end SGI */

#if HP700_ARCH
#undef FORSTR_METHOD_F
#define FORSTR_METHOD_F 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM _forstr_dummy1,_forstr_dummy2
#define FORSTR_DEF int _forstr_dummy1,_forstr_dummy2;
#define FORSTR_BLOCK
#endif

#if AXP_UNIX_ARCH
#include <varargs.h>
#undef FORSTR_METHOD_G
#define FORSTR_METHOD_G 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM _forparam
#define FORSTR_DEF long _forparam;
#define FORSTR_BLOCK va_list _valist;int _fordummy=(__builtin_va_start(_valist,_forparam,0),0);
#endif

#if PPC_LINUX_ARCH
#include <varargs.h>
/* Redefine va_dcl to remove "register" from the decl, to avoid a warning. */
#undef va_dcl
#define va_dcl int va_alist; ...
#undef FORSTR_METHOD_I
#define FORSTR_METHOD_I 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM _forstr_dummy1, _forstr_dummy2, _forstr_dummy3, _forstr_dummy4
#define FORSTR_DEF int _forstr_dummy1, _forstr_dummy2, _forstr_dummy3, _forstr_dummy4;
#define FORSTR_BLOCK
#endif

#if ANY_OSX_ARCH
#undef FORSTR_METHOD_B
#define FORSTR_METHOD_B 1
#undef FORSTR_METHOD_STDARG
#define FORSTR_METHOD_STDARG 1
#define FORSTR_PARAM _forstr_dummy
#define FORSTR_DEF int _forstr_dummy;
#define FORSTR_BLOCK
#endif

/************************************************************************/
/* Methods B, C, E, F, G, H and I require the number of arguments to be	*/
/* known.  A and D do not, so in those cases we can skip some code.	*/
/************************************************************************/

#if FORSTR_METHOD_B+FORSTR_METHOD_C+FORSTR_METHOD_E+FORSTR_METHOD_F+FORSTR_METHOD_G+FORSTR_METHOD_H+FORSTR_METHOD_I
#define FORSTR_NARGS 1
#else
#define FORSTR_NARGS 0
#endif

/************************************************************************/
/* Prototypes for new-style variadic routines.  Should only be used	*/
/* internally by the RTL, but since they're referenced in the macros	*/
/* below, prototypes are needed.					*/
/************************************************************************/

#ifdef _NO_PROTO
void  v2_sc2for();
void  v2_sc2for_array();
void  v2_sfor2c();
int   v2_sfor2c_array();
int   v2_sfor2len();
int   v2_sfor2len_inc();
char *v2_sfor2ptr();
#else
void v2_sc2for(
                char *c_string,
                int max_length,
                char *for_string,
                void *argptr,
                int nargs,
                int argno,
                int strno,
		va_list *param,
		int *which);
void v2_sc2for_array(
                char *c_string,
                int len,
                int nelements,
                char *for_string,
                int *max_length,
                void *argptr,
                int nargs,
                int argno,
                int strno,
		va_list *param,
		int *which);
void v2_sfor2c(
                char *c_string,
                int len,
                char *for_string,
                void *argptr,
                int nargs,
                int argno,
                int strno,
		va_list *param,
		int *which);
int v2_sfor2c_array(
                char **c_string,
                int *max_length,
                int nelements,
                char *for_string,
                void *argptr,
                int nargs,
                int argno,
                int strno,
		va_list *param,
		int *which);
int v2_sfor2len(
                char *for_string,
                void *argptr,
                int nargs,
                int argno,
                int strno,
		va_list *param,
		int *which);
char *v2_sfor2ptr(
                char *for_string);
#endif

/************************************************************************/
/* New-style API for constant-argument routines.  This API is entirely	*/
/* implemented by macros here.  Note that the FORSTR macros are		*/
/* different than for the old-style API, but the new ones accomplish	*/
/* the same thing (forcing arguments into the stack, etc.) so they are	*/
/* equally valid.							*/
/* A few platforms above do NOT define ENABLE_NEW_API.  These probably	*/
/* could do so, but have not been tested and are sufficiently different	*/
/* that I am unsure.  The new API cannot be used without ENABLE_NEW_API	*/
/* being defined; this effectively removes support for those platforms	*/
/* until/unless it can be tested there.  (not all platforms with STDARG	*/
/* turned on have been tested either, but they are sufficiently similar	*/
/* to others to believe it will work fine).  It s also likely that	*/
/* these off platforms could be activated by recasting FORSTR_PARAM	*/
/* into ANSI-style arguments, but that is not done here.		*/
/************************************************************************/

#ifdef ENABLE_NEW_API

/* Ensure that va_copy is defined for all platforms. */
#ifndef va_copy
#ifdef __va_copy
#define va_copy(a,b) __va_copy(a,b)
#else
#define va_copy(a,b) a=(b)
#endif
#endif

#define ZFORSTR_PARAM ...
#define ZFORSTR_BLOCK va_list _sfor2c_tmp_valist; int _sfor2c_tmp_which;

#define zsfor2c(c_string, max_length, for_string, argptr, nargs, argno, strno, last_arg) \
 { va_start(_sfor2c_tmp_valist, last_arg); \
   _sfor2c_tmp_which = 0; \
   v2_sfor2c(c_string, max_length, for_string, argptr, nargs, argno, strno, &_sfor2c_tmp_valist, &_sfor2c_tmp_which); \
   va_end(_sfor2c_tmp_valist); }

/* TBD:  is return value for zsfor2c_array important??!!!!*/

#define zsfor2c_array(c_string, len, nelements, for_string, argptr, nargs, argno, strno, last_arg) \
 { va_start(_sfor2c_tmp_valist, last_arg); \
   _sfor2c_tmp_which = 0; \
   v2_sfor2c_array(c_string, len, nelements, for_string, argptr, nargs, argno, strno, &_sfor2c_tmp_valist, &_sfor2c_tmp_which); \
   va_end(_sfor2c_tmp_valist); }

#define zsc2for(c_string, max_length, for_string, argptr, nargs, argno, strno, last_arg) \
 { va_start(_sfor2c_tmp_valist, last_arg); \
   _sfor2c_tmp_which = 0; \
   v2_sc2for(c_string, max_length, for_string, argptr, nargs, argno, strno, &_sfor2c_tmp_valist, &_sfor2c_tmp_which); \
   va_end(_sfor2c_tmp_valist); }

#define zsc2for_array(c_string, maxlen, nelements, for_string, len, argptr, nargs, argno, strno, last_arg) \
 { va_start(_sfor2c_tmp_valist, last_arg); \
   _sfor2c_tmp_which = 0; \
   v2_sc2for_array(c_string, maxlen, nelements, for_string, len, argptr, nargs, argno, strno, &_sfor2c_tmp_valist, &_sfor2c_tmp_which); \
   va_end(_sfor2c_tmp_valist); }

#define zsfor2ptr(for_string) v2_sfor2ptr(for_string)

#define zsfor2len(length, for_string, argptr, nargs, argno, strno, last_arg) \
 { va_start(_sfor2c_tmp_valist, last_arg); \
   _sfor2c_tmp_which = 0; \
   length = v2_sfor2len(for_string, argptr, nargs, argno, strno, &_sfor2c_tmp_valist, &_sfor2c_tmp_which); \
   va_end(_sfor2c_tmp_valist); }

#endif /* FORSTR_METHOD_STDARG */

#endif /* _FTNBRIDGE_H */

