/* $Header: /usr/people/sam/tiff/libtiff/RCS/tiffcompat.h,v 1.21 92/03/30 18:31:03 sam Exp $ */

/*
 * Copyright (c) 1990, 1991, 1992 Sam Leffler
 * Copyright (c) 1991, 1992 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#ifndef _COMPAT_
#define	_COMPAT_
/*
 * This file contains a hodgepodge of definitions and
 * declarations that are needed to provide compatibility
 * between the native system and the base UNIX implementation
 * that the library assumes (~4BSD).  In particular, you
 * can override the standard i/o interface (read/write/lseek)
 * by redefining the ReadOK/WriteOK/SeekOK macros to your
 * liking.
 *
 * NB: This file is a mess.
 */
 
/*
 *   This file will be integrated into VICAR, so we will use
 *		the xvmaininc. defininitions to control everything
 */

#include "xvmaininc.h"
#include <string.h>

#if MAC_MPW_ARCH
#  define USE_VARARGS		0
#  define USE_PROTOTYPES	1
#else
#  define USE_CONST		0
#  define USE_VARARGS		0	/* was 1 but varags no longer supported (rgd 3/2010) */
#  define USE_PROTOTYPES	1	/* needed for USE_VARARGS */
#endif

#if VAX_ARCH+ALPHA_ARCH+MAC_MPW_ARCH
#  define BSDTYPES
#endif

#if VAX_ARCH+ALPHA_ARCH+DECSTATION_ARCH+CRAY_ARCH
#  define HAVE_IEEEFP 0
#else
#  define HAVE_IEEEFP 1
#endif
 
#if (defined(__STDC__) || defined(__EXTENDED__)) && !defined(USE_PROTOTYPES)
#define	USE_PROTOTYPES	1
#define	USE_CONST	1
#endif
#include <limits.h>
#if defined(applec) || defined(USE_CONST)
#if !USE_CONST
#define const
#else /* !applec */
#if !USE_CONST && !defined(const)
#define	const
#endif
#endif
#endif /* !applec */
#ifdef THINK_C
#include <unix.h>
#include <string.h>
#endif
#if defined(THINK_C) || defined(applec)
#include <math.h>
#endif
#if USE_PROTOTYPES
#include <stdio.h>
#endif
#if defined(applec) || defined(vms)
#include <types.h>
#else
#if !defined(THINK_C) && !defined(applec)
#include <sys/types.h>
#endif
#endif
#ifdef vms
#include <file.h>
#include <unixio.h>
#else
#include <fcntl.h>
#endif
#if defined(THINK_C) || defined(applec)
#include <stdlib.h>
#endif

/*
 * Workarounds for BSD lseek definitions.
 */
#if defined(SYSV) || defined(vms)
#if defined(SYSV)
#include <unistd.h>
#endif
#define	L_SET	SEEK_SET
#define	L_INCR	SEEK_CUR
#define	L_XTND	SEEK_END
#endif
#ifndef L_SET
#define L_SET	0
#define L_INCR	1
#define L_XTND	2
#endif

/*
 * SVID workarounds for BSD bit
 * string manipulation routines.
 */
/* #if defined(SYSV) || defined(THINK_C) || defined(applec) || defined(vms) || defined (SOLARIS) */
#define	mybzero(dst,len)		memset((char *)dst, 0, len)
#define	mybcopy(src,dst,len)	memcpy((char *)dst, (char *)src, len)
#define	mybcmp(src, dst, len)	memcmp((char *)dst, (char *)src, len)
/* #endif */

/*
 * The BSD typedefs are used throughout the library.
 * If your system doesn't have them in <sys/types.h>,
 * then define BSDTYPES in your Makefile.
 */
#ifdef BSDTYPES
typedef	unsigned char u_char;
typedef	unsigned short u_short;
typedef	unsigned int u_int;
/*typedef	unsigned long u_long; SEE tiff_u_long in tiff.h */
#endif

/*
 * Return an open file descriptor or -1.
 */
#if defined(applec) || defined(THINK_C)
#define	TIFFOpenFile(name, mode, prot)	open(name, mode)
#else
#if defined(MSDOS)
#define	TIFFOpenFile(name, mode, prot)	open(name, mode|O_BINARY, prot)
#else
#define	TIFFOpenFile(name, mode, prot)	open(name, mode, prot)
#endif
#endif



#if USE_PROTOTYPES
extern	long TIFFGetFileSize(int fd);
extern int TIFFWriteShort(int fd, short *n);
extern int TIFFReadShort(int fd, short *n);
extern void TIFFPackShort(short *s,long n);
extern void TIFFUnPackShort(short *s,long n);
#else
extern	long TIFFGetFileSize();
extern int TIFFWriteShort();
extern int TIFFReadShort();
extern void TIFFPackShort();
extern void TIFFUnPackShort();
#endif

#ifdef MMAP_SUPPORT
/*
 * Mapped file support.
 *
 * TIFFMapFileContents must map the entire file into
 *     memory and return the address of the mapped
 *     region and the size of the mapped region.
 * TIFFUnmapFileContents does the inverse operation.
 */
#if USE_PROTOTYPES
extern	int TIFFMapFileContents(int fd, char **paddr, long *psize);
extern	void TIFFUnmapFileContents(char *addr, long size);
#else
extern	int TIFFMapFileContents();
extern	void TIFFUnmapFileContents();
#endif
#endif

/*
 * Mac workaround to handle the file
 * extension semantics of lseek.
 */
#ifdef applec
#define	lseek	mpw_lseek
extern long mpw_lseek(int, long, int);
#else
extern	long lseek();
#endif

/*
 * Default Read/Seek/Write definitions.
 */

#ifndef ReadOK
#if defined(THINK_C) || defined(transputer) || defined(vms)
#define	ReadOK(fd, buf, size)	(BigRead(fd, (char *)buf, (unsigned long)size))
#else
#define	ReadOK(fd, buf, size)	(read(fd, (char *)buf, size) == size)
#endif
#endif
#ifndef SeekOK
#define	SeekOK(fd, off)	(lseek(fd, (long)off, L_SET) == (long)off)
#endif
#ifndef WriteOK
#if defined(THINK_C) || defined(transputer) || defined(vms)
#define	WriteOK(fd, buf, size)	(BigWrite(fd, (char *)buf, (unsigned long)size))
#else
#define	WriteOK(fd, buf, size)	(write(fd, (char *)buf, size) == size)
#endif
#endif

#if defined(THINK_C) || defined(transputer) || defined(vms)
#if USE_PROTOTYPES
int BigWrite(int fd,char *buf, unsigned long nbytes);
int BigRead(int fd,char *buf, unsigned long nbytes);
#else
int BigWrite();
int BigRead();
#endif
#endif

#include <stdlib.h>

/*
 * dblparam_t is the type that a double precision
 * floating point value will have on the parameter
 * stack (when coerced by the compiler).
 */
#ifdef applec
typedef extended dblparam_t;
#else
typedef double dblparam_t;
#endif

/*
 * Varargs parameter list handling...YECH!!!!
 */
#if defined(__STDC__) && !defined(USE_VARARGS)
#define	USE_VARARGS	0
#endif

#if defined(USE_VARARGS)
#if USE_VARARGS
#ifdef va_dcl
#undef va_dcl
#undef va_arg
#undef va_start
#undef va_end
#endif
#include <varargs.h>
#define	VA_START(ap, parmN)	va_start(ap)
#else
#include <stdarg.h>
#define	VA_START(ap, parmN)	va_start(ap, parmN)
#endif
#endif /* defined(USE_VARARGS) */

/* We can't assume that the internal size of some */
/* structs are the same as the TIFF size of the   */
/* same fields                                    */

#define TIFFShortSize  2L
#define TIFFLongSize   4L
#define TIFFDirSize   12L  /* two "shorts" & two "longs" */
#define TIFFHdrSize    8L  /* two "shorts" & two "longs" */

#endif /* _COMPAT_ */
