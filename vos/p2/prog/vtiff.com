$!****************************************************************************
$!
$! Build proc for MIPL module vtiff
$! VPACK Version 1.9, Thursday, November 03, 2011, 14:42:24
$!
$! Execute by entering:		$ @vtiff
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module vtiff ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vtiff.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("vtiff.imake") .nes. ""
$   then
$      vimake vtiff
$      purge vtiff.bld
$   else
$      if F$SEARCH("vtiff.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vtiff
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vtiff.bld "STD"
$   else
$      @vtiff.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vtiff.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vtiff.com -mixed -
	-s t_aux.c t_cmpat.c t_cmpr.c t_dirw.c t_dirr.c t_diri.c t_dump.c -
	   t_err.c t_file.c t_lzw.c t_mach.c t_pack.c t_prnt.c t_strp.c -
	   t_swab.c t_tile.c t_vers.c prototypes.h tiffcompat.h tiffiop.h -
	   tiff.h tiffio.h vtiff.c optimal_color.c -
	-p vtiff.pdf -
	-t tstvtiff.pdf -
	-i vtiff.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create t_aux.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_aux.c,v 1.8 92/03/27 14:53:02 sam Exp $";
#endif

/*
 * Copyright (c) 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * Auxiliary Support Routines.
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <math.h>
#include <strings.h>
#include <string.h>

/*
 * Like TIFFGetField, but return any default
 * value if the tag is not present in the directory.
 *
 * NB:	We use the value in the directory, rather than
 *	explcit values so that defaults exist only one
 *	place in the library -- in TIFFDefaultDirectory.
 */
TIFFVGetFieldDefaulted(tif, tag, ap)
	TIFF *tif;
	int tag;
	va_list ap;
{
	TIFFDirectory *td = &tif->tif_dir;
	int i;

	if (TIFFVGetField(tif, tag, ap))
		return (1);
	switch (tag) {
	case TIFFTAG_SUBFILETYPE:
		*va_arg(ap, u_short *) = td->td_subfiletype;
		return (1);
	case TIFFTAG_BITSPERSAMPLE:
		*va_arg(ap, u_short *) = td->td_bitspersample;
		return (1);
	case TIFFTAG_THRESHHOLDING:
		*va_arg(ap, u_short *) = td->td_threshholding;
		return (1);
	case TIFFTAG_FILLORDER:
		*va_arg(ap, u_short *) = td->td_fillorder;
		return (1);
	case TIFFTAG_ORIENTATION:
		*va_arg(ap, u_short *) = td->td_orientation;
		return (1);
	case TIFFTAG_SAMPLESPERPIXEL:
		*va_arg(ap, u_short *) = td->td_samplesperpixel;
		return (1);
	case TIFFTAG_ROWSPERSTRIP:
		*va_arg(ap, tiff_u_long *) = td->td_rowsperstrip;
		return (1);
	case TIFFTAG_MINSAMPLEVALUE:
		*va_arg(ap, u_short *) = td->td_minsamplevalue;
		return (1);
	case TIFFTAG_MAXSAMPLEVALUE:
		*va_arg(ap, u_short *) = td->td_maxsamplevalue;
		return (1);
	case TIFFTAG_PLANARCONFIG:
		*va_arg(ap, u_short *) = td->td_planarconfig;
		return (1);
	case TIFFTAG_GROUP4OPTIONS:
		*va_arg(ap, tiff_u_long *) = td->td_group4options;
		return (1);
	case TIFFTAG_RESOLUTIONUNIT:
		*va_arg(ap, u_short *) = td->td_resolutionunit;
		return (1);
	case TIFFTAG_PREDICTOR:
		*va_arg(ap, u_short *) = td->td_predictor;
		return (1);
#ifdef CMYK_SUPPORT
	case TIFFTAG_DOTRANGE:
		*va_arg(ap, u_short *) = 0;
		*va_arg(ap, u_short *) = (1<<td->td_bitspersample)-1;
		return (1);
	case TIFFTAG_INKSET:
		*va_arg(ap, u_short *) = td->td_inkset;
		return (1);
#endif
	case TIFFTAG_TILEDEPTH:
		*va_arg(ap, tiff_u_long *) = td->td_tiledepth;
		return (1);
	case TIFFTAG_DATATYPE:
		*va_arg(ap, u_short *) = td->td_sampleformat-1;
		return (1);
	case TIFFTAG_IMAGEDEPTH:
		*va_arg(ap, u_short *) = td->td_imagedepth;
		return (1);
#ifdef YCBCR_SUPPORT
	case TIFFTAG_YCBCRCOEFFICIENTS:
		if (!td->td_ycbcrcoeffs) {
			td->td_ycbcrcoeffs = (float *)malloc(3*sizeof (float));
			/* defaults are from CCIR Recommendation 601-1 */
			td->td_ycbcrcoeffs[0] = .299;
			td->td_ycbcrcoeffs[1] = .587;
			td->td_ycbcrcoeffs[2] = .114;
		}
		*va_arg(ap, float **) = td->td_ycbcrcoeffs;
		return (1);
	case TIFFTAG_YCBCRSUBSAMPLING:
		*va_arg(ap, u_short *) = td->td_ycbcrsubsampling[0];
		*va_arg(ap, u_short *) = td->td_ycbcrsubsampling[1];
		return (1);
	case TIFFTAG_YCBCRPOSITIONING:
		*va_arg(ap, u_short *) = td->td_ycbcrpositioning;
		return (1);
#endif
#ifdef COLORIMETRY_SUPPORT
	case TIFFTAG_TRANSFERFUNCTION:
		if (!td->td_transferfunction[0]) {
			u_short **tf = td->td_transferfunction;
			int n = 1<<td->td_bitspersample;

			tf[0] = (u_short *)malloc(n * sizeof (u_short));
			tf[0][0] = 0;
			for (i = 1; i < n; i++)
				tf[0][i] = (u_short)
				    floor(65535.*pow(i/(n-1.), 2.2) + .5);
			for (i = 1; i < (int)td->td_samplesperpixel; i++) {
				tf[i] = (u_short *)malloc(n * sizeof (u_short));
				mybcopy(tf[0], tf[i], n * sizeof (u_short));
			}
		}
		for (i = 0; i < (int)td->td_samplesperpixel; i++)
			*va_arg(ap, u_short **) = td->td_transferfunction[i];
		return (1);
	case TIFFTAG_REFERENCEBLACKWHITE:
		if (!td->td_refblackwhite) {
			td->td_refblackwhite = (float *)
			    malloc(2*td->td_samplesperpixel * sizeof (float));
			for (i = 0; i < (int)td->td_samplesperpixel; i++) {
				td->td_refblackwhite[2*i+0] = 0;
				td->td_refblackwhite[2*i+1] = 1L<<td->td_bitspersample;
			}
		}
		*va_arg(ap, float **) = td->td_refblackwhite;
		return (1);
#endif
	}
	return (0);
}

/*
 * Like TIFFGetField, but return any default
 * value if the tag is not present in the directory.
 */
/*VARARGS2*/
DECLARE2V(TIFFGetFieldDefaulted, TIFF*, tif, int, tag)
{
	int ok;
	va_list ap;

	VA_START(ap, tag);
	ok =  TIFFVGetFieldDefaulted(tif, tag, ap);
	va_end(ap);
	return (ok);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_cmpat.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_compat.c,v 1.15 92/03/11 09:16:49 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library Compatibility Routines.
 */
#include "tiffiop.h"

#if defined(unix) || defined(__unix) || defined(MSDOS) || defined(vms) || defined(transputer) || defined(__APPLE__)
#ifdef vms
#include stat
#else
#include <sys/stat.h>
#endif

long
TIFFGetFileSize(fd)
	int fd;
{
	struct stat sb;

	return (fstat(fd, &sb) < 0 ? 0 : sb.st_size);
}
#else
#if defined(THINK_C) || defined(applec)
long
TIFFGetFileSize(int fd)
{
	long pos, eof;
	
	pos = lseek(fd, 0, SEEK_CUR);
	eof = lseek(fd, 0, SEEK_END);
	lseek(fd, pos, SEEK_SET);
	return(eof);
}
#endif /* THINK_C || applec || transputer */
#endif

#if defined(THINK_C) || defined(transputer) || defined(vms)

/* Some unix IO routines cant handle >64k bytes */
#ifdef THINK_C
#define BLOCK_SIZE 31*1024
#else
#define BLOCK_SIZE 512
#endif

int BigWrite(int fd,char *buf, unsigned long nbytes)
{
      register unsigned int num;
      register char *outbuf;
      register long outc=0;
      char *endbuf;
      
      endbuf=buf+nbytes;
      for (outbuf=buf;outbuf<endbuf;outbuf+=num)
      {
          num = (nbytes > BLOCK_SIZE) ? BLOCK_SIZE : nbytes;
      	  if (write(fd, outbuf, num)!=num) return (0);
          nbytes -= num;
      }
	  return (1);
}

int BigRead(int fd,char *buf, unsigned long nbytes)
{
	  register unsigned int num;
      register char *inbuf;
      register long outc=0;
      char *endbuf;
      
      endbuf=buf+nbytes;
      for (inbuf=buf;inbuf<endbuf;inbuf+=num)
      {
          num = (nbytes > BLOCK_SIZE) ? BLOCK_SIZE : nbytes;
      	  if (read(fd, inbuf, num)!=num) return (0);
          nbytes -= num;
      }
	  return (1);
}

#endif

#ifdef transputer

int TIFFWriteShort(fd,n)
int fd;
short *n;
{
	if (write(fd, (( char *)n), 2L)!=2)
		return (0);
	return (1);
}

int TIFFReadShort(fd,n)
int fd;
short *n;
{
	short inval=0;

	if (read(fd, (( char *)&inval), 2L) != 2)
		return (0);
	*n = inval;
	return (1);
}

void TIFFPackShort(s,n)
short *s;
long n;
{
	register char *iptr,*optr;
	long i;
	
	optr=(char *)s;
	iptr=((char *)s);
	
	for(i=0;i<n;i++)
	{
		*optr++ = *iptr++;
		*optr++ = *iptr++;
		iptr+=2;
	}
}

void TIFFUnPackShort(s,n)
short *s;
long n;
{
	register char *iptr,*optr;
	long i;
	
	optr=((char *)s) + 4*n -1;
	iptr=((char *)s) + 2*n -1;
	
	for(i=0;i<n;i++)
	{
		*optr-- = 0;
		*optr-- = 0;
		*optr-- = *iptr--;
		*optr-- = *iptr--;
	}
	
}
#else

int TIFFWriteShort(fd,n)
int fd;
short *n;
{
	if (write(fd, (char *)n, 2L)!=2)
		return (0);
	return (1);
}

int TIFFReadShort(fd,n)
int fd;
short *n;
{
	if (read(fd, (char *)n, 2L) != 2)
		return (0);
	return (1);
}

void TIFFPackShort(s,n)
short *s;
long n;
{
	/* do nothing */
}

void TIFFUnPackShort(s,n)
short *s;
long n;
{
	/* do nothing */
}

#endif


#if (defined(unix) || defined(__unix)) && defined(MMAP_SUPPORT)
#include <sys/mman.h>

int
TIFFMapFileContents(fd, pbase, psize)
	int fd;
	char **pbase;
	long *psize;
{
	long size = TIFFGetFileSize(fd);
	if (size != -1) {
		*pbase = (char *) mmap(0, size, PROT_READ, MAP_SHARED, fd, 0);
		if (*pbase != (char *)-1) {
			*psize = size;
			return (1);
		}
	}
	return (0);
}

void
TIFFUnmapFileContents(base, size)
	char *base;
	long size;
{
	(void) munmap(base, size);
}
#endif /* (defined(unix) || defined(__unix)) && defined(MMAP_SUPPORT) */

#if defined(VMS) && defined(MMAP_SUPPORT)
#include <fab.h>
#include <secdef.h>

/*
 * Table for storing information on current open sections. 
 * (You may want to substitute a linked list...)
 */
#define MAX_MAPPED 100
static int no_mapped = 0;
static struct {
	void *base;
	void *top;
	unsigned channel;
} map_table[MAX_MAPPED];

/* 
 * This routine maps a file into a private section. Note that this 
 * method of accessing a file is by far the fastest under VMS.
 * The routine may fail (i.e. return 0) for several reasons, for
 * example:
 * - There is no more room for storing the info on sections.
 * - The process is out of open file quota, channels, ...
 * - fd does not describe an opened file.
 * - The file is already opened for write access by this process
 *   or another process
 * - There is no free "hole" in virtual memory that fits the
 *   size of the file
 */
int
TIFFMapFileContents(fd, pbase, psize)
	int fd;
	char **pbase;
	long *psize;
{
	char name[256];
	struct FAB fab;
	unsigned short channel;
	void *inadr[2], *retadr[2];
	unsigned long status;
	long size;
	
	if (no_mapped >= MAX_MAPPED)
		return(0);
	/*
	 * We cannot use a file descriptor, we
	 * must open the file once more.
	 */
	if (getname(fd, name, 1) == NULL)
		return(0);
	/* prepare the FAB for a user file open */
	fab = cc$rms_fab;
	fab.fab$v_ufo = 1;
	fab.fab$b_fac = FAB$M_GET;
	fab.fab$b_shr = FAB$M_SHRGET;
	fab.fab$l_fna = name;
	fab.fab$b_fns = strlen(name);
	status = sys$open(&fab);	/* open file & get channel number */
	if ((status&1) == 0)
		return(0);
	channel = (unsigned short)fab.fab$l_stv;
	inadr[0] = inadr[1] = &channel;	/* just an address in P0 space */
	/*
	 * Map the blocks of the file up to
	 * the EOF block into virtual memory.
	 */
	size = TIFFGetFileSize(fd);
	status = sys$crmpsc(inadr, retadr, 0, SEC$M_EXPREG, 0,0,0, channel,
		howmany(size,512), 0,0,0);
	if ((status&1) == 0)
		return(0);
	*pbase = retadr[0];		/* starting virtual address */
	/*
	 * Use the size of the file up to the
	 * EOF mark for UNIX compatibility.
	 */
	*psize = size;
	/* Record the section in the table */
	map_table[no_mapped].base = retadr[0];
	map_table[no_mapped].top = retadr[1];
	map_table[no_mapped].channel = channel;
	no_mapped++;

        return(1);
}

/*
 * This routine unmaps a section from the virtual address space of 
 * the process, but only if the base was the one returned from a
 * call to TIFFMapFileContents.
 */
void
TIFFUnmapFileContents(base, size)
	char *base;
	long size;
{
	void *inadr[2];
	int i, j;
	
	/* Find the section in the table */
	for (i = 0;i < no_mapped; i++) {
		if (map_table[i].base == base) {
			/* Unmap the section */
			inadr[1] = base;
			inadr[0] = map_table[i].top;
			sys$deltva(inadr, 0, 0);
			sys$dassgn(map_table[i].channel);
			/* Remove this section from the list */
			for (j = i+1; j < no_mapped; j++)
				map_table[j-1] = map_table[j];
			no_mapped--;
			return;
		}
	}
}
#endif /* defined(VMS) && defined(MMAP_SUPPORT) */

#if defined(applec)
#include <ioctl.h>
#include <Files.h>
#undef lseek

long
mpw_lseek(int fd, long offset, int whence)
{
	long filepos, filesize, newpos;
	short macfd;
	
	if ((filepos = lseek(fd, 0, SEEK_CUR)) < 0 ||
	    (filesize = lseek(fd, 0, SEEK_END)) < 0)
		return (EOF);
	newpos = offset + (whence == SEEK_SET ? 0 : 
			   whence == SEEK_CUR ? filepos :
						filesize);
	if (newpos > filesize)
		if (ioctl(fd, FIOREFNUM, (long*)&macfd) == -1 ||
		    SetEOF(macfd, newpos) != 0)
			return (EOF);
	return (lseek(fd, newpos, SEEK_SET));
}
#endif /* applec */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_cmpr.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_compress.c,v 1.26 92/02/10 19:06:13 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library
 *
 * Compression Scheme Configuration Support.
 */
#include "tiffiop.h"

#if USE_PROTOTYPES
extern	int TIFFInitDumpMode(TIFF*);
#ifdef PACKBITS_SUPPORT
extern	int TIFFInitPackBits(TIFF*);
#endif
#ifdef CCITT_SUPPORT
extern	int TIFFInitCCITTRLE(TIFF*), TIFFInitCCITTRLEW(TIFF*);
extern	int TIFFInitCCITTFax3(TIFF*), TIFFInitCCITTFax4(TIFF*);
#endif
#ifdef THUNDER_SUPPORT
extern	int TIFFInitThunderScan(TIFF*);
#endif
#ifdef NEXT_SUPPORT
extern	int TIFFInitNeXT(TIFF*);
#endif
#ifdef LZW_SUPPORT
extern	int TIFFInitLZW(TIFF*);
#endif
#ifdef JPEG_SUPPORT
extern	int TIFFInitJPEG(TIFF*);
#endif
#else
extern	int TIFFInitDumpMode();
#ifdef PACKBITS_SUPPORT
extern	int TIFFInitPackBits();
#endif
#ifdef CCITT_SUPPORT
extern	int TIFFInitCCITTRLE(), TIFFInitCCITTRLEW();
extern	int TIFFInitCCITTFax3(), TIFFInitCCITTFax4();
#endif
#ifdef THUNDER_SUPPORT
extern	int TIFFInitThunderScan();
#endif
#ifdef NEXT_SUPPORT
extern	int TIFFInitNeXT();
#endif
#ifdef LZW_SUPPORT
extern	int TIFFInitLZW();
#endif
#ifdef JPEG_SUPPORT
extern	int TIFFInitJPEG();
#endif
#endif

struct cscheme {
	char*	name;
	int	scheme;
#if USE_PROTOTYPES
	int	(*init)(TIFF*);
#else
	int	(*init)();
#endif
};
static const struct cscheme CompressionSchemes[] = {
    { "Null",		COMPRESSION_NONE,	TIFFInitDumpMode },
#ifdef LZW_SUPPORT
    { "LZW",		COMPRESSION_LZW,	TIFFInitLZW },
#endif
#ifdef PACKBITS_SUPPORT
    { "PackBits",	COMPRESSION_PACKBITS,	TIFFInitPackBits },
#endif
#ifdef THUNDER_SUPPORT
    { "ThunderScan",	COMPRESSION_THUNDERSCAN,TIFFInitThunderScan },
#endif
#ifdef NEXT_SUPPORT
    { "NeXT",		COMPRESSION_NEXT,	TIFFInitNeXT },
#endif
#ifdef JPEG_SUPPORT
    { "JPEG",		COMPRESSION_JPEG,	TIFFInitJPEG },
#endif
#ifdef CCITT_SUPPORT
    { "CCITT RLE",	COMPRESSION_CCITTRLE,	TIFFInitCCITTRLE },
    { "CCITT RLE/W",	COMPRESSION_CCITTRLEW,	TIFFInitCCITTRLEW },
    { "CCITT Group3",	COMPRESSION_CCITTFAX3,	TIFFInitCCITTFax3 },
    { "CCITT Group4",	COMPRESSION_CCITTFAX4,	TIFFInitCCITTFax4 },
#endif
};
#define	NSCHEMES (sizeof (CompressionSchemes) / sizeof (CompressionSchemes[0]))

static struct cscheme const *
findScheme(scheme)
	int scheme;
{
	register struct cscheme const *c;

	for (c = CompressionSchemes; c < &CompressionSchemes[NSCHEMES]; c++)
		if (c->scheme == scheme)
			return (c);
	return ((struct cscheme const *)0);
}

static int
TIFFNoEncode(tif, method)
	TIFF *tif;
	char *method;
{
	struct cscheme const *c = findScheme(tif->tif_dir.td_compression);
	TIFFError(tif->tif_name,
	    "%s %s encoding is not implemented", c->name, method);
	return (-1);
}

int
TIFFNoRowEncode(tif, pp, cc, s)
	TIFF *tif;
	u_char *pp;
	tiff_u_long cc;
	u_int s;
{
	return (TIFFNoEncode(tif, "scanline"));
}

int
TIFFNoStripEncode(tif, pp, cc, s)
	TIFF *tif;
	u_char *pp;
	tiff_u_long cc;
	u_int s;
{
	return (TIFFNoEncode(tif, "strip"));
}

int
TIFFNoTileEncode(tif, pp, cc, s)
	TIFF *tif;
	u_char *pp;
	tiff_u_long cc;
	u_int s;
{
	return (TIFFNoEncode(tif, "tile"));
}

int
TIFFNoDecode(tif, method)
	TIFF *tif;
	char *method;
{
	struct cscheme const *c = findScheme(tif->tif_dir.td_compression);
	TIFFError(tif->tif_name,
	    "%s %s decoding is not implemented", c->name, method);
	return (-1);
}

int
TIFFNoRowDecode(tif, pp, cc, s)
	TIFF *tif;
	u_char *pp;
	tiff_u_long cc;
	u_int s;
{
	return (TIFFNoDecode(tif, "scanline"));
}

int
TIFFNoStripDecode(tif, pp, cc, s)
	TIFF *tif;
	u_char *pp;
	tiff_u_long cc;
	u_int s;
{
	return (TIFFNoDecode(tif, "strip"));
}

int
TIFFNoTileDecode(tif, pp, cc, s)
	TIFF *tif;
	u_char *pp;
	tiff_u_long cc;
	u_int s;
{
	return (TIFFNoDecode(tif, "tile"));
}

TIFFSetCompressionScheme(tif, scheme)
	TIFF *tif;
	int scheme;
{
	struct cscheme const *c = findScheme(scheme);

	if (!c) {
		TIFFError(tif->tif_name,
		    "Unknown data compression algorithm %u (0x%x)",
		    scheme, scheme);
		return (0);
	}
	tif->tif_predecode = NULL;
	tif->tif_decoderow = TIFFNoRowDecode;
	tif->tif_decodestrip = TIFFNoStripDecode;
	tif->tif_decodetile = TIFFNoTileDecode;
	tif->tif_preencode = NULL;
	tif->tif_postencode = NULL;
	tif->tif_encoderow = TIFFNoRowEncode;
	tif->tif_encodestrip = TIFFNoStripEncode;
	tif->tif_encodetile = TIFFNoTileEncode;
	tif->tif_close = NULL;
	tif->tif_seek = NULL;
	tif->tif_cleanup = NULL;
	tif->tif_flags &= ~TIFF_NOBITREV;
	tif->tif_options = 0;
	return ((*c->init)(tif));
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_dirw.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_dir.c,v 1.110 92/03/06 11:59:49 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * Directory Tag Get & Set Routines.
 * (and also some miscellaneous stuff)
 *
 * NB: Beware of the varargs declarations for routines in
 *     this file.  The names and types of variables has been
 *     carefully chosen to make things work with compilers that
 *     are busted in one way or another (e.g. SGI/MIPS).
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <string.h>
#include <strings.h>

#if USE_PROTOTYPES
extern	int TIFFSetCompressionScheme(TIFF *, int);
extern	int TIFFDefaultDirectory(TIFF*);
extern  int TIFFFreeDirectory(TIFF*);
#else
extern	int TIFFSetCompressionScheme();
extern	int TIFFDefaultDirectory();
extern	int TIFFFreeDirectory();
#endif

/** from dir_writ **/

#if HAVE_IEEEFP
#define	TIFFCvtNativeToIEEEFloat(tif, n, fp)
#endif

#if USE_PROTOTYPES
static	TIFFWriteNormalTag(TIFF*, TIFFDirEntry*, TIFFFieldInfo*);
static	TIFFSetupShortLong(TIFF *, u_short, TIFFDirEntry *, tiff_u_long);
static	TIFFSetupShortPair(TIFF *, u_short, TIFFDirEntry *);
static	TIFFWriteRational(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, float);
static	TIFFWritePerSampleShorts(TIFF *, u_short, TIFFDirEntry *);
static	TIFFWriteShortTable(TIFF *, u_short, TIFFDirEntry *, int, u_short **);
static	TIFFWriteShortArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, u_short *);
static	TIFFWriteLongArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, tiff_u_long *);
static	TIFFWriteRationalArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, float *);
static	TIFFWriteFloatArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, float *);
static	TIFFWriteString(TIFF *, u_short, TIFFDirEntry *, char *);
static	TIFFFloatToRational(float,tiff_u_long *);
static	CARTLinkDirectory(register TIFF *);
static	TIFFgetfraction(dblparam_t, tiff_u_long *, tiff_u_long *, dblparam_t,int);
extern  int TIFFFlushData1(TIFF*);
extern  int CARTWriteDirectory(TIFF*);
extern  int	TIFFFreeDirectory(TIFF*);
extern  int	TIFFDefaultDirectory(TIFF*);
#ifdef JPEG_SUPPORT
static	TIFFWriteJPEGQTables(TIFF *, TIFFDirEntry *);
static	TIFFWriteJPEGCTables(TIFF *, u_short, TIFFDirEntry *, u_char **);
#endif
#ifdef COLORIMETRY_SUPPORT
static	TIFFWriteTransferFunction(TIFF*, TIFFDirEntry*);
#endif
static	TIFFWriteData(TIFF *, TIFFDirEntry *, char *);
static	TIFFLinkDirectory(TIFF *);
static  TIFFWriteTIFFDir(int fd, TIFFDirEntry* dir, int n);
#else
static	TIFFWriteNormalTag();
static	TIFFSetupShortLong();
static	TIFFSetupShortPair();
static	TIFFWriteRational();
static	TIFFWritePerSampleShorts();
static	TIFFWriteShortTable();
static	TIFFWriteShortArray();
static	TIFFWriteLongArray();
static	TIFFWriteRationalArray();
static	TIFFWriteFloatArray();
static	TIFFWriteString();
static	TIFFFloatToRational();
static	CARTLinkDirectory();
static	TIFFgetfraction();
extern  int TIFFFlushData1();
extern  int CARTWriteDirectory();
extern  int	TIFFFreeDirectory();
extern  int	TIFFDefaultDirectory();
#ifdef JPEG_SUPPORT
static	TIFFWriteJPEGQTables();
static	TIFFWriteJPEGCTables();
#endif
static	TIFFWriteData();
static	TIFFLinkDirectory();
static  TIFFWriteTIFFDir();
#endif

#define	WriteRationalPair(type, tag1, v1, tag2, v2) {		\
	if (!TIFFWriteRational(tif, type, tag1, dir, v1))	\
		goto bad;					\
	if (!TIFFWriteRational(tif, type, tag2, dir+1, v2))	\
		goto bad;					\
	dir++;							\
}

/** end dir_writ **/


static
DECLARE2(setString, char**, cpp, char*, cp)
{
	if (*cpp)
		free(*cpp), *cpp = 0;
	if (cp) {
		int len = strlen(cp)+1;
		if (*cpp = malloc(len))
			mybcopy(cp, *cpp, len);
	}
}

static
DECLARE3(setShortArray, u_short**, wpp, u_short*, wp, long, n)
{
	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	n *= sizeof (u_short);
	if (wp && (*wpp = (u_short *)malloc(n)))
		mybcopy(wp, *wpp, n);
}

static
DECLARE3(setLongArray, tiff_u_long**, wpp, tiff_u_long*, wp, long, n)
{
	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	n *= sizeof (tiff_u_long);
	if (wp && (*wpp = (tiff_u_long *)malloc(n)))
		mybcopy(wp, *wpp, n);
}

static
DECLARE3(setFloatArray, float**, wpp, float*, wp, long, n)
{
	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	n *= sizeof (float);
	if (wp && (*wpp = (float *)malloc(n)))
		mybcopy(wp, *wpp, n);
}

#ifdef JPEG_SUPPORT
/*
 * Install a JPEG Quantization table.
 * Note that we reorder the elements
 * of the array in the zig-zag order
 * that is expected by the compression code
 * and that is to be stored in the file.
 */
static
DECLARE3(setJPEGQTable, u_char***, wpp, u_char**, wp, int, nc)
{
	static u_char zigzag[64] = {
	    0,  1,  5,  6, 14, 15, 27, 28,
	    2,  4,  7, 13, 16, 26, 29, 42,
	    3,  8, 12, 17, 25, 30, 41, 43,
	    9, 11, 18, 24, 31, 40, 44, 53,
	   10, 19, 23, 32, 39, 45, 52, 54,
	   20, 22, 33, 38, 46, 51, 55, 60,
	   21, 34, 37, 47, 50, 56, 59, 61,
	   35, 36, 48, 49, 57, 58, 62, 63
	};
	char *tab;
	int i, j;

	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	*wpp = (u_char **)
	    malloc(nc * (sizeof (u_char *) + 64*sizeof (u_char)));
	tab = (((char *)*wpp) + nc*sizeof (u_short *));
	for (i = 0; i < nc; i++) {
		(*wpp)[i] = (u_char *)tab;
		for (j = 0; j < 64; j++)
			tab[zigzag[j]] = wp[i][j];
		tab += 64*sizeof (u_char);
	}
}

/*
 * Install a JPEG Coefficient table.
 */
static
DECLARE3(setJPEGCTable, u_char***, cpp, u_char**, cp, int, nc)
{
	u_char *tab;
	int i, j, nw;

	if (*cpp)
		free(*cpp), *cpp = 0;
	/*
	 * Calculate the size of the table by counting
	 * the number of codes specified in the bits array.
	 */
	nw = 0;
	for (i = 0; i < nc; i++) {
		nw += 16;		/* 16 bytes for bits array */
		for (j = 0; j < 16; j++)/* sum up count of codes */
			nw += cp[i][j];
	}
	*cpp = (u_char **)malloc(nc*sizeof (u_char *) + nw);
	tab = ((u_char *)*cpp) + nc*sizeof (u_char *);
	/*
	 * Setup internal array and copy user data.
	 */
	for (i = 0; i < nc; i++) {
		(*cpp)[i] = tab;
		for (nw = 16, j = 0; j < 16; j++)
			nw += cp[i][j];
		mybcopy(cp[i], tab, nw);
		tab += nw;
	}
}
#endif

#ifdef CARTOGRAPHIC_SUPPORT
static
CARTSetField(tif, tag, ap)
TIFF *tif;
int tag;
va_list ap;
{
	TIFFDirectory *td = &tif->tif_dir;

	switch (tag) {
		case TIFFTAG_CARTO_IFD_OFFSET:
			td->td_carto_ifd_offset = va_arg(ap, tiff_u_long);
			break;
		case CARTTAG_PROJECTIONTYPE:
			td->td_projectiontype = va_arg(ap, int);
			break;
		case CARTTAG_PROJ_XPOS:
			td->td_proj_xpos = va_arg(ap, tiff_u_long);
			break;
		case CARTTAG_PROJ_YPOS:
			td->td_proj_ypos = va_arg(ap, tiff_u_long);
			break;
		case CARTTAG_LATITUDE:
			td->td_latitude = va_arg(ap, tiff_u_long);
			break;
		case CARTTAG_LONGITUDE:
			td->td_longitude = va_arg(ap, tiff_u_long);
			break;
		case CARTTAG_XPIXPERANGLE:
			td->td_xpixperangle = va_arg(ap, dblparam_t);
			break;
		case CARTTAG_YPIXPERANGLE:
			td->td_ypixperangle = va_arg(ap, dblparam_t);
			break;
		default:
			return(0);
	}

	tif->tif_flags |= TIFF_DIRTYCART;
	return (1);
}
#endif /* CARTO */

static
TIFFSetField1(tif, tag, ap)
	TIFF *tif;
	int tag;
	va_list ap;
{
	TIFFDirectory *td = &tif->tif_dir;
	int i, status = 1;
	long v;

	switch (tag) {
	case TIFFTAG_SUBFILETYPE:
		td->td_subfiletype = va_arg(ap, tiff_u_long);
		break;
	case TIFFTAG_IMAGEWIDTH:
		td->td_imagewidth = va_arg(ap, tiff_u_long);
		break;
	case TIFFTAG_IMAGELENGTH:
		td->td_imagelength = va_arg(ap, tiff_u_long);
		break;
	case TIFFTAG_BITSPERSAMPLE:
		td->td_bitspersample = va_arg(ap, int);
		break;
	case TIFFTAG_COMPRESSION:
		v = va_arg(ap, int) & 0xffff;
		/*
		 * If we're changing the compression scheme,
		 * the notify the previous module so that it
		 * can cleanup any state it's setup.
		 */
		if (TIFFFieldSet(tif, FIELD_COMPRESSION)) {
			if (td->td_compression == v)
				break;
			if (tif->tif_cleanup)
				(*tif->tif_cleanup)(tif);
		}
		/*
		 * Setup new compression routine state.
		 */
		if (status = TIFFSetCompressionScheme(tif, v))
			td->td_compression = v;
		break;
	case TIFFTAG_PHOTOMETRIC:
		td->td_photometric = va_arg(ap, int);
		break;
	case TIFFTAG_THRESHHOLDING:
		td->td_threshholding = va_arg(ap, int);
		break;
	case TIFFTAG_FILLORDER:
		v = va_arg(ap, int);
		if (v != FILLORDER_LSB2MSB && v != FILLORDER_MSB2LSB)
			goto badvalue;
		td->td_fillorder = v;
		break;
	case TIFFTAG_DOCUMENTNAME:
		setString(&td->td_documentname, va_arg(ap, char *));
		break;
	case TIFFTAG_ARTIST:
		setString(&td->td_artist, va_arg(ap, char *));
		break;
	case TIFFTAG_DATETIME:
		setString(&td->td_datetime, va_arg(ap, char *));
		break;
	case TIFFTAG_HOSTCOMPUTER:
		setString(&td->td_hostcomputer, va_arg(ap, char *));
		break;
	case TIFFTAG_IMAGEDESCRIPTION:
		setString(&td->td_imagedescription, va_arg(ap, char *));
		break;
	case TIFFTAG_MAKE:
		setString(&td->td_make, va_arg(ap, char *));
		break;
	case TIFFTAG_MODEL:
		setString(&td->td_model, va_arg(ap, char *));
		break;
	case TIFFTAG_SOFTWARE:
		setString(&td->td_software, va_arg(ap, char *));
		break;
	case TIFFTAG_ORIENTATION:
		v = va_arg(ap, int);
		if (v < ORIENTATION_TOPLEFT || ORIENTATION_LEFTBOT < v) {
			TIFFWarning(tif->tif_name,
			    "Bad value %ld for \"%s\" tag ignored",
			    v, TIFFFieldWithTag(tag)->field_name);
		} else
			td->td_orientation = v;
		break;
	case TIFFTAG_SAMPLESPERPIXEL:
		/* XXX should cross check -- e.g. if pallette, then 1 */
		v = va_arg(ap, int);
		if (v == 0)
			goto badvalue;
		if (v > 4) {
			TIFFError(tif->tif_name,
			    "Cannot handle %ld-channel data", v); 
			status = 0;
		} else
			td->td_samplesperpixel = v;
		break;
	case TIFFTAG_ROWSPERSTRIP:
		v = va_arg(ap, tiff_u_long);
		if (v == 0)
			goto badvalue;
		td->td_rowsperstrip = v;
		if (!TIFFFieldSet(tif, FIELD_TILEDIMENSIONS)) {
			td->td_tilelength = v;
			td->td_tilewidth = td->td_imagewidth;
		}
		break;
	case TIFFTAG_MINSAMPLEVALUE:
		td->td_minsamplevalue = va_arg(ap, int) & 0xffff;
		break;
	case TIFFTAG_MAXSAMPLEVALUE:
		td->td_maxsamplevalue = va_arg(ap, int) & 0xffff;
		break;
	case TIFFTAG_XRESOLUTION:
		td->td_xresolution = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_YRESOLUTION:
		td->td_yresolution = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_PLANARCONFIG:
		v = va_arg(ap, int);
		if (v != PLANARCONFIG_CONTIG && v != PLANARCONFIG_SEPARATE)
			goto badvalue;
		td->td_planarconfig = v;
		break;
	case TIFFTAG_PAGENAME:
		setString(&td->td_pagename, va_arg(ap, char *));
		break;
	case TIFFTAG_XPOSITION:
		td->td_xposition = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_YPOSITION:
		td->td_yposition = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_GROUP3OPTIONS:
		td->td_group3options = va_arg(ap, tiff_u_long);
		break;
	case TIFFTAG_GROUP4OPTIONS:
		td->td_group4options = va_arg(ap, tiff_u_long);
		break;
	case TIFFTAG_RESOLUTIONUNIT:
		v = va_arg(ap, int);
		if (v < RESUNIT_NONE || RESUNIT_CENTIMETER < v)
			goto badvalue;
		td->td_resolutionunit = v;
		break;
	case TIFFTAG_PAGENUMBER:
		td->td_pagenumber[0] = va_arg(ap, int);
		td->td_pagenumber[1] = va_arg(ap, int);
		break;
	case TIFFTAG_HALFTONEHINTS:
		td->td_halftonehints[0] = va_arg(ap, int);
		td->td_halftonehints[1] = va_arg(ap, int);
		break;
	case TIFFTAG_COLORMAP:
		v = 1L<<td->td_bitspersample;
		setShortArray(&td->td_colormap[0], va_arg(ap, u_short *), v);
		setShortArray(&td->td_colormap[1], va_arg(ap, u_short *), v);
		setShortArray(&td->td_colormap[2], va_arg(ap, u_short *), v);
		break;
	case TIFFTAG_PREDICTOR:
		td->td_predictor = va_arg(ap, int);
		break;
	case TIFFTAG_EXTRASAMPLES:
		v = va_arg(ap, int);
		if (v > (int)td->td_samplesperpixel)
			goto badvalue;
		if (v != 1)			/* XXX */
			goto badvalue;		/* XXX */
		v = va_arg(ap, int);
		if (v != EXTRASAMPLE_ASSOCALPHA)/* XXX */
			goto badvalue;		/* XXX */
		td->td_matteing = 1;
		break;
	case TIFFTAG_MATTEING:
		td->td_matteing = va_arg(ap, int);
		break;
	case TIFFTAG_BADFAXLINES:
		td->td_badfaxlines = va_arg(ap, tiff_u_long);
		break;
	case TIFFTAG_CLEANFAXDATA:
		td->td_cleanfaxdata = va_arg(ap, int);
		break;
	case TIFFTAG_CONSECUTIVEBADFAXLINES:
		td->td_badfaxrun = va_arg(ap, tiff_u_long);
		break;
	case TIFFTAG_TILEWIDTH:
		v = va_arg(ap, tiff_u_long);
		if (v % 8)
			goto badvalue;
		td->td_tilewidth = v;
		tif->tif_flags |= TIFF_ISTILED;
		break;
	case TIFFTAG_TILELENGTH:
		v = va_arg(ap, tiff_u_long);
		if (v % 8)
			goto badvalue;
		td->td_tilelength = v;
		tif->tif_flags |= TIFF_ISTILED;
		break;
	case TIFFTAG_TILEDEPTH:
		v = va_arg(ap, tiff_u_long);
		if (v == 0)
			goto badvalue;
		td->td_tiledepth = v;
		break;
	case TIFFTAG_DATATYPE:
	case TIFFTAG_SAMPLEFORMAT:
		v = va_arg(ap, int);
		if (tag == TIFFTAG_DATATYPE && v == 0)
			v = SAMPLEFORMAT_VOID;
		if (v < SAMPLEFORMAT_INT || SAMPLEFORMAT_VOID < v)
			goto badvalue;
		td->td_sampleformat = v;
		break;
	case TIFFTAG_IMAGEDEPTH:
		td->td_imagedepth = va_arg(ap, tiff_u_long);
		break;
#ifdef YCBCR_SUPPORT
	case TIFFTAG_YCBCRCOEFFICIENTS:
		setFloatArray(&td->td_ycbcrcoeffs, va_arg(ap, float *), 3);
		break;
	case TIFFTAG_YCBCRPOSITIONING:
		td->td_ycbcrpositioning = va_arg(ap, int);
		break;
	case TIFFTAG_YCBCRSUBSAMPLING:
		td->td_ycbcrsubsampling[0] = va_arg(ap, int);
		td->td_ycbcrsubsampling[1] = va_arg(ap, int);
		break;
#endif
#ifdef JPEG_SUPPORT
	case TIFFTAG_JPEGPROC:
		td->td_jpegproc = va_arg(ap, int);
		break;
	case TIFFTAG_JPEGRESTARTINTERVAL:
		td->td_jpegrestartinterval = va_arg(ap, int);
		break;
	case TIFFTAG_JPEGQTABLES:
		setJPEGQTable(&td->td_qtab, va_arg(ap, u_char **),
		    td->td_samplesperpixel);
		break;
	case TIFFTAG_JPEGDCTABLES:
		setJPEGCTable(&td->td_dctab, va_arg(ap, u_char **),
		    td->td_samplesperpixel);
		break;
	case TIFFTAG_JPEGACTABLES:
		setJPEGCTable(&td->td_actab, va_arg(ap, u_char **),
		    td->td_samplesperpixel);
		break;
#endif
#ifdef COLORIMETRY_SUPPORT
	case TIFFTAG_WHITEPOINT:
		setFloatArray(&td->td_whitepoint, va_arg(ap, float *), 2);
		break;
	case TIFFTAG_PRIMARYCHROMATICITIES:
		setFloatArray(&td->td_primarychromas, va_arg(ap, float *), 6);
		break;
	case TIFFTAG_TRANSFERFUNCTION:
		for (i = 0; i < (int)td->td_samplesperpixel; i++)
		    setShortArray(&td->td_transferfunction[i],
			va_arg(ap, u_short *), 1L<<td->td_bitspersample);
		break;
	case TIFFTAG_REFERENCEBLACKWHITE:
		/* XXX should check for null range */
		v = 2 * td->td_samplesperpixel;
		setFloatArray(&td->td_refblackwhite, va_arg(ap, float *), v);
		break;
#endif
#ifdef CMYK_SUPPORT
	case TIFFTAG_INKSET:
		td->td_inkset = va_arg(ap, int);
		break;
	case TIFFTAG_DOTRANGE:
		/* XXX should check for null range */
		td->td_dotrange[0] = va_arg(ap, int);
		td->td_dotrange[1] = va_arg(ap, int);
		break;
	case TIFFTAG_INKNAMES:
		setString(&td->td_inknames, va_arg(ap, char *));
		break;
	case TIFFTAG_TARGETPRINTER:
		setString(&td->td_targetprinter, va_arg(ap, char *));
		break;
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	case TIFFTAG_CARTO_IFD_OFFSET:
	case CARTTAG_PROJECTIONTYPE:
	case CARTTAG_PROJ_XPOS:
	case CARTTAG_PROJ_YPOS:
	case CARTTAG_LATITUDE:
	case CARTTAG_LONGITUDE:
	case CARTTAG_XPIXPERANGLE:
	case CARTTAG_YPIXPERANGLE:
		CARTSetField(tif,tag,ap);
		break;
#endif /* CARTO */
	default:
		TIFFError(tif->tif_name,
		    "Internal error, tag value botch, tag \"%s\"",
		    TIFFFieldWithTag(tag)->field_name);
		status = 0;
		break;
	}
	if (status) {
		TIFFSetFieldBit(tif, TIFFFieldWithTag(tag)->field_bit);
		tif->tif_flags |= TIFF_DIRTYDIRECT;
	}
	va_end(ap);
	return (status);
badvalue:
	TIFFError(tif->tif_name, "%ld: Bad value for \"%s\"", v,
	    TIFFFieldWithTag(tag)->field_name);
	va_end(ap);
	return (0);
}

/*
 * Return 1/0 according to whether or not
 * it is permissible to set the tag's value.
 * Note that we allow ImageLength to be changed
 * so that we can append and extend to images.
 * Any other tag may not be altered once writing
 * has commenced, unless its value has no effect
 * on the format of the data that is written.
 */
static
OkToChangeTag(tif, tag)
	TIFF *tif;
	int tag;
{
	if (tag != TIFFTAG_IMAGELENGTH &&
	    (tif->tif_flags & TIFF_BEENWRITING)) {
		TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);
		/*
		 * Consult info table to see if tag can be changed
		 * after we've started writing.  We only allow changes
		 * to those tags that don't/shouldn't affect the
		 * compression and/or format of the data.
		 */
		if (fip && !fip->field_oktochange)
			return (0);
	}
	return (1);
}

/*
 * Record the value of a field in the
 * internal directory structure.  The
 * field will be written to the file
 * when/if the directory structure is
 * updated.
 */
/*VARARGS2*/
DECLARE2V(TIFFSetField, TIFF*, tif, int, tag)
{
	int status = 0;

	if (OkToChangeTag(tif, tag)) {
		va_list ap;

		VA_START(ap, tag);
		status = TIFFSetField1(tif, tag, ap);
		va_end(ap);
	} else {
		TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);
		if (fip)
			TIFFError("TIFFSetField",
			    "%s: Cannot modify tag \"%s\" while writing",
			    tif->tif_name, fip->field_name);
	}
	return (status);
}

/*
 * Like TIFFSetField, but taking a varargs
 * parameter list.  This routine is useful
 * for building higher-level interfaces on
 * top of the library.
 */
TIFFVSetField(tif, tag, ap)
	TIFF *tif;
	int tag;
	va_list ap;
{
	int status = 0;

	if (!OkToChangeTag(tif, tag)) {
		TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);
		if (fip)
			TIFFError("TIFFVSetField",
			    "%s: Cannot modify tag \"%s\" while writing",
			    tif->tif_name, fip->field_name);
	} else
		status = TIFFSetField1(tif, tag, ap);
	return (status);
}

static
TIFFGetField1(td, tag, ap)
	TIFFDirectory *td;
	int tag;
	va_list ap;
{

	switch (tag) {
	case TIFFTAG_SUBFILETYPE:
		*va_arg(ap, tiff_u_long *) = td->td_subfiletype;
		break;
	case TIFFTAG_IMAGEWIDTH:
		*va_arg(ap, tiff_u_long *) = td->td_imagewidth;
		break;
	case TIFFTAG_IMAGELENGTH:
		*va_arg(ap, tiff_u_long *) = td->td_imagelength;
		break;
	case TIFFTAG_BITSPERSAMPLE:
		*va_arg(ap, u_short *) = td->td_bitspersample;
		break;
	case TIFFTAG_COMPRESSION:
		*va_arg(ap, u_short *) = td->td_compression;
		break;
	case TIFFTAG_PHOTOMETRIC:
		*va_arg(ap, u_short *) = td->td_photometric;
		break;
	case TIFFTAG_THRESHHOLDING:
		*va_arg(ap, u_short *) = td->td_threshholding;
		break;
	case TIFFTAG_FILLORDER:
		*va_arg(ap, u_short *) = td->td_fillorder;
		break;
	case TIFFTAG_DOCUMENTNAME:
		*va_arg(ap, char **) = td->td_documentname;
		break;
	case TIFFTAG_ARTIST:
		*va_arg(ap, char **) = td->td_artist;
		break;
	case TIFFTAG_DATETIME:
		*va_arg(ap, char **) = td->td_datetime;
		break;
	case TIFFTAG_HOSTCOMPUTER:
		*va_arg(ap, char **) = td->td_hostcomputer;
		break;
	case TIFFTAG_IMAGEDESCRIPTION:
		*va_arg(ap, char **) = td->td_imagedescription;
		break;
	case TIFFTAG_MAKE:
		*va_arg(ap, char **) = td->td_make;
		break;
	case TIFFTAG_MODEL:
		*va_arg(ap, char **) = td->td_model;
		break;
	case TIFFTAG_SOFTWARE:
		*va_arg(ap, char **) = td->td_software;
		break;
	case TIFFTAG_ORIENTATION:
		*va_arg(ap, u_short *) = td->td_orientation;
		break;
	case TIFFTAG_SAMPLESPERPIXEL:
		*va_arg(ap, u_short *) = td->td_samplesperpixel;
		break;
	case TIFFTAG_ROWSPERSTRIP:
		*va_arg(ap, tiff_u_long *) = td->td_rowsperstrip;
		break;
	case TIFFTAG_MINSAMPLEVALUE:
		*va_arg(ap, u_short *) = td->td_minsamplevalue;
		break;
	case TIFFTAG_MAXSAMPLEVALUE:
		*va_arg(ap, u_short *) = td->td_maxsamplevalue;
		break;
	case TIFFTAG_XRESOLUTION:
		*va_arg(ap, float *) = td->td_xresolution;
		break;
	case TIFFTAG_YRESOLUTION:
		*va_arg(ap, float *) = td->td_yresolution;
		break;
	case TIFFTAG_PLANARCONFIG:
		*va_arg(ap, u_short *) = td->td_planarconfig;
		break;
	case TIFFTAG_XPOSITION:
		*va_arg(ap, float *) = td->td_xposition;
		break;
	case TIFFTAG_YPOSITION:
		*va_arg(ap, float *) = td->td_yposition;
		break;
	case TIFFTAG_PAGENAME:
		*va_arg(ap, char **) = td->td_pagename;
		break;
	case TIFFTAG_GROUP3OPTIONS:
		*va_arg(ap, tiff_u_long *) = td->td_group3options;
		break;
	case TIFFTAG_GROUP4OPTIONS:
		*va_arg(ap, tiff_u_long *) = td->td_group4options;
		break;
	case TIFFTAG_RESOLUTIONUNIT:
		*va_arg(ap, u_short *) = td->td_resolutionunit;
		break;
	case TIFFTAG_PAGENUMBER:
		*va_arg(ap, u_short *) = td->td_pagenumber[0];
		*va_arg(ap, u_short *) = td->td_pagenumber[1];
		break;
	case TIFFTAG_HALFTONEHINTS:
		*va_arg(ap, u_short *) = td->td_halftonehints[0];
		*va_arg(ap, u_short *) = td->td_halftonehints[1];
		break;
	case TIFFTAG_COLORMAP:
		*va_arg(ap, u_short **) = td->td_colormap[0];
		*va_arg(ap, u_short **) = td->td_colormap[1];
		*va_arg(ap, u_short **) = td->td_colormap[2];
		break;
	case TIFFTAG_PREDICTOR:
		*va_arg(ap, u_short *) = td->td_predictor;
		break;
	case TIFFTAG_STRIPOFFSETS:
	case TIFFTAG_TILEOFFSETS:
		*va_arg(ap, tiff_u_long **) = td->td_stripoffset;
		break;
	case TIFFTAG_STRIPBYTECOUNTS:
	case TIFFTAG_TILEBYTECOUNTS:
		*va_arg(ap, tiff_u_long **) = td->td_stripbytecount;
		break;
	case TIFFTAG_MATTEING:
		*va_arg(ap, u_short *) = td->td_matteing;
		break;
	case TIFFTAG_EXTRASAMPLES:
		*va_arg(ap, u_short *) = td->td_matteing;
		*va_arg(ap, u_short **) = &td->td_matteing;
		break;
	case TIFFTAG_BADFAXLINES:
		*va_arg(ap, tiff_u_long *) = td->td_badfaxlines;
		break;
	case TIFFTAG_CLEANFAXDATA:
		*va_arg(ap, u_short *) = td->td_cleanfaxdata;
		break;
	case TIFFTAG_CONSECUTIVEBADFAXLINES:
		*va_arg(ap, tiff_u_long *) = td->td_badfaxrun;
		break;
	case TIFFTAG_TILEWIDTH:
		*va_arg(ap, tiff_u_long *) = td->td_tilewidth;
		break;
	case TIFFTAG_TILELENGTH:
		*va_arg(ap, tiff_u_long *) = td->td_tilelength;
		break;
	case TIFFTAG_TILEDEPTH:
		*va_arg(ap, tiff_u_long *) = td->td_tiledepth;
		break;
	case TIFFTAG_DATATYPE:
		*va_arg(ap, u_short *) =
		    (td->td_sampleformat == SAMPLEFORMAT_VOID ?
			0 : td->td_sampleformat);
		break;
	case TIFFTAG_SAMPLEFORMAT:
		*va_arg(ap, u_short *) = td->td_sampleformat;
		break;
	case TIFFTAG_IMAGEDEPTH:
		*va_arg(ap, tiff_u_long *) = td->td_imagedepth;
		break;
#ifdef YCBCR_SUPPORT
	case TIFFTAG_YCBCRCOEFFICIENTS:
		*va_arg(ap, float **) = td->td_ycbcrcoeffs;
		break;
	case TIFFTAG_YCBCRPOSITIONING:
		*va_arg(ap, u_short *) = td->td_ycbcrpositioning;
		break;
	case TIFFTAG_YCBCRSUBSAMPLING:
		*va_arg(ap, u_short *) = td->td_ycbcrsubsampling[0];
		*va_arg(ap, u_short *) = td->td_ycbcrsubsampling[1];
		break;
#endif
#ifdef JPEG_SUPPORT
	case TIFFTAG_JPEGPROC:
		*va_arg(ap, u_short *) = td->td_jpegproc;
		break;
	case TIFFTAG_JPEGRESTARTINTERVAL:
		*va_arg(ap, u_short *) = td->td_jpegrestartinterval;
		break;
	case TIFFTAG_JPEGQTABLES:
		*va_arg(ap, u_char ***) = td->td_qtab;
		break;
	case TIFFTAG_JPEGDCTABLES:
		*va_arg(ap, u_char ***) = td->td_dctab;
		break;
	case TIFFTAG_JPEGACTABLES:
		*va_arg(ap, u_char ***) = td->td_actab;
		break;
#endif
#ifdef COLORIMETRY_SUPPORT
	case TIFFTAG_WHITEPOINT:
		*va_arg(ap, float **) = td->td_whitepoint;
		break;
	case TIFFTAG_PRIMARYCHROMATICITIES:
		*va_arg(ap, float **) = td->td_primarychromas;
		break;
	case TIFFTAG_TRANSFERFUNCTION: {
		int i;
		for (i = 0; i < (int)td->td_samplesperpixel; i++)
			*va_arg(ap, u_short **) = td->td_transferfunction[i];
		break;
	}
	case TIFFTAG_REFERENCEBLACKWHITE:
		*va_arg(ap, float **) = td->td_refblackwhite;
		break;
#endif
#ifdef CMYK_SUPPORT
	case TIFFTAG_INKSET:
		*va_arg(ap, u_short *) = td->td_inkset;
		break;
	case TIFFTAG_DOTRANGE:
		*va_arg(ap, u_short *) = td->td_dotrange[0];
		*va_arg(ap, u_short *) = td->td_dotrange[1];
		break;
	case TIFFTAG_INKNAMES:
		*va_arg(ap, char **) = td->td_inknames;
		break;
	case TIFFTAG_TARGETPRINTER:
		*va_arg(ap, char **) = td->td_targetprinter;
		break;
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	case TIFFTAG_CARTO_IFD_OFFSET:
		*va_arg(ap, tiff_u_long *) = td->td_carto_ifd_offset;
		break;
	case CARTTAG_PROJECTIONTYPE:
		*va_arg(ap, u_short *) = td->td_projectiontype ;
		break;
	case CARTTAG_PROJ_XPOS:
		*va_arg(ap, tiff_u_long *) = td->td_proj_xpos ;
		break;
	case CARTTAG_PROJ_YPOS:
		*va_arg(ap, tiff_u_long *) = td->td_proj_ypos ;
		break;
	case CARTTAG_LATITUDE:
		*va_arg(ap, tiff_u_long *) = td->td_latitude ;
		break;
	case CARTTAG_LONGITUDE:
		*va_arg(ap, tiff_u_long *) = td->td_longitude ;
		break;
	case CARTTAG_XPIXPERANGLE:
		*va_arg(ap, float *) = td->td_xpixperangle ;
		break;
	case CARTTAG_YPIXPERANGLE:
		*va_arg(ap, float *) = td->td_ypixperangle ;
		break;
#endif
	default:
		TIFFError("TIFFGetField1",
		    "Internal error, no value returned for tag \"%s\"",
		    TIFFFieldWithTag(tag)->field_name);
		break;
	}
	va_end(ap);
}

/*
 * Return the value of a field in the
 * internal directory structure.
 */
/*VARARGS2*/
DECLARE2V(TIFFGetField, TIFF*, tif, int, tag)
{
	TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);

	if (fip) {
		u_short bit = fip->field_bit;
		if (bit != FIELD_IGNORE && TIFFFieldSet(tif, bit)) {
			va_list ap;
			VA_START(ap, tag);
			(void) TIFFGetField1(&tif->tif_dir, tag, ap);
			va_end(ap);
			return (1);
		}
	} else
		TIFFError("TIFFGetField", "Unknown field, tag 0x%x", tag);
	return (0);
}

/*
 * Like TIFFGetField, but taking a varargs
 * parameter list.  This routine is useful
 * for building higher-level interfaces on
 * top of the library.
 */
TIFFVGetField(tif, tag, ap)
	TIFF *tif;
	int tag;
	va_list ap;
{
	TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);

	if (fip) {
		u_short bit = fip->field_bit;
		if (bit != FIELD_IGNORE && TIFFFieldSet(tif, bit)) {
			(void) TIFFGetField1(&tif->tif_dir, tag, ap);
			return (1);
		}
	} else
		TIFFError("TIFFGetField", "Unknown field, tag 0x%x", tag);
	return (0);
}

/*
 * Internal interface to TIFFGetField...
 */
int
/*VARARGS2*/
DECLARE2V(_TIFFgetfield, TIFFDirectory*, td, int, tag)
{
	va_list ap;

	VA_START(ap, tag);
	(void) TIFFGetField1(td, tag, ap);
	va_end(ap);
	return (1);
}

#define	CleanupField(member) {		\
    if (td->member) {			\
	free((char *)td->member);	\
	td->member = 0;			\
    }					\
}

/*
 * Release storage associated with a directory.
 */
TIFFFreeDirectory(tif)
	TIFF *tif;
{
	register TIFFDirectory *td = &tif->tif_dir;

	CleanupField(td_colormap[0]);
	CleanupField(td_colormap[1]);
	CleanupField(td_colormap[2]);
	CleanupField(td_documentname);
	CleanupField(td_artist);
	CleanupField(td_datetime);
	CleanupField(td_hostcomputer);
	CleanupField(td_imagedescription);
	CleanupField(td_make);
	CleanupField(td_model);
	CleanupField(td_software);
	CleanupField(td_pagename);
#ifdef YCBCR_SUPPORT
	CleanupField(td_ycbcrcoeffs);
#endif
#ifdef JPEG_SUPPORT
	CleanupField(td_qtab);
	CleanupField(td_dctab);
	CleanupField(td_actab);
#endif
#ifdef CMYK_SUPPORT
	CleanupField(td_inknames);
	CleanupField(td_targetprinter);
#endif
#ifdef COLORIMETRY_SUPPORT
	CleanupField(td_whitepoint);
	CleanupField(td_primarychromas);
	CleanupField(td_refblackwhite);
	CleanupField(td_transferfunction[0]);
	CleanupField(td_transferfunction[1]);
	CleanupField(td_transferfunction[2]);
	CleanupField(td_transferfunction[3]);
#endif
	CleanupField(td_stripoffset);
	CleanupField(td_stripbytecount);
}
#undef CleanupField

/*
 * Setup a default directory structure.
 */
TIFFDefaultDirectory(tif)
	TIFF *tif;
{
	register TIFFDirectory *td = &tif->tif_dir;

	mybzero((char *)td, sizeof (*td));
	td->td_fillorder = FILLORDER_MSB2LSB;
	td->td_bitspersample = 1;
	td->td_threshholding = THRESHHOLD_BILEVEL;
	td->td_orientation = ORIENTATION_TOPLEFT;
	td->td_samplesperpixel = 1;
	td->td_predictor = 1;
	td->td_rowsperstrip = 0xffffffff;
	td->td_tilewidth = 0xffffffff;
	td->td_tilelength = 0xffffffff;
	td->td_tiledepth = 1;
	td->td_resolutionunit = RESUNIT_INCH;
	td->td_sampleformat = SAMPLEFORMAT_VOID;
	td->td_imagedepth = 1;
#ifdef YCBCR_SUPPORT
	td->td_ycbcrsubsampling[0] = 2;
	td->td_ycbcrsubsampling[1] = 2;
	td->td_ycbcrpositioning = YCBCRPOSITION_CENTERED;
#endif
#ifdef CMYK_SUPPORT
	td->td_inkset = INKSET_CMYK;
#endif
	(void) TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
	/*
	 * NB: The directory is marked dirty as a result of setting
	 * up the default compression scheme.  However, this really
	 * isn't correct -- we want TIFF_DIRTYDIRECT to be set only
	 * if the user does something.  We could just do the setup
	 * by hand, but it seems better to use the normal mechanism
	 * (i.e. TIFFSetField).
	 */
	tif->tif_flags &= ~(TIFF_DIRTYDIRECT);
	return (1);
}

/*
 * Set the n-th directory as the current directory.
 * NB: Directories are numbered starting at 0.
 */
TIFFSetDirectory(tif, dirn)
	register TIFF *tif;
	int dirn;
{
	static char module[] = "TIFFSetDirectory";
	u_short dircount;
	long nextdir;
	int n;

	nextdir = tif->tif_header.tiff_diroff;
	for (n = dirn; n > 0 && nextdir != 0; n--) {
		if (!SeekOK(tif->tif_fd, nextdir) ||
		    !TIFFReadShort(tif->tif_fd, (short *)&dircount)) {
			TIFFError(module, "%s: Error fetching directory count",
			    tif->tif_name);
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabShort(&dircount);
		lseek(tif->tif_fd, dircount*TIFFDirSize, L_INCR);
		if (!ReadOK(tif->tif_fd, &nextdir, TIFFLongSize)) {
			TIFFError(module, "%s: Error fetching directory link",
			    tif->tif_name);
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabLong((tiff_u_long *)&nextdir);
	}
	tif->tif_nextdiroff = nextdir;
	/*
	 * Set curdir to the actual directory index.  The
	 * -1 is because TIFFReadDirectory will increment
	 * tif_curdir after successfully reading the directory.
	 */
	tif->tif_curdir = (dirn - n) - 1;
	return (TIFFReadDirectory(tif));
}



/* start dir_writ */

static	long dataoff;
/*
 * Write the contents of the current directory
 * to the specified file.  This routine doesn't
 * handle overwriting a directory with auxiliary
 * storage that's been changed.
 */
TIFFWriteDirectory(tif)
	TIFF *tif;
{
	short dircount, tag;
	int nfields, dirsize;
	char *data;
	TIFFFieldInfo *fip;
	TIFFDirEntry *dir;
	TIFFDirectory *td;
	tiff_u_long b, fields[sizeof (td->td_fieldsset) / sizeof (tiff_u_long)];

	if (tif->tif_mode == O_RDONLY)
		return (1);
	/*
	 * Clear write state so that subsequent images with
	 * different characteristics get the right buffers
	 * setup for them.
	 */
	if (tif->tif_flags & TIFF_POSTENCODE) {
		tif->tif_flags &= ~TIFF_POSTENCODE;
		if (tif->tif_postencode && !(*tif->tif_postencode)(tif)) {
			TIFFError(tif->tif_name,
			    "Error post-encoding before directory write");
			return (0);
		}
	}
	if (tif->tif_close)
		(*tif->tif_close)(tif);
	if (tif->tif_cleanup)
		(*tif->tif_cleanup)(tif);
	/*
	 * Flush any data that might have been written
	 * by the compression close+cleanup routines.
	 */
	if (tif->tif_rawcc > 0 && !TIFFFlushData1(tif)) {
		TIFFError(tif->tif_name,
		    "Error flushing data before directory write");
		return (0);
	}
	if ((tif->tif_flags & TIFF_MYBUFFER) && tif->tif_rawdata) {
		free(tif->tif_rawdata);
		tif->tif_rawdata = NULL;
		tif->tif_rawcc = 0;
	}
	tif->tif_flags &= ~(TIFF_BEENWRITING|TIFF_BUFFERSETUP|TIFF_ISTILED);
#ifdef CARTOGRAPHIC_SUPPORT
	/*
	 * Write out the private Carto Directory, if values have been set.
	 */
	 if (tif->tif_flags & TIFF_DIRTYCART)
	 	CARTWriteDirectory(tif);
	tif->tif_flags &= ~TIFF_HASCARTTAGS;
#endif

	td = &tif->tif_dir;
	/*
	 * Size the directory so that we can calculate
	 * offsets for the data items that aren't kept
	 * in-place in each field.
	 */
	nfields = 0;
	for (b = 0; b <= FIELD_LAST; b++)
		if (TIFFFieldSet(tif, b))
			nfields += (b < FIELD_SUBFILETYPE ? 2 : 1);
	dirsize = nfields * sizeof (TIFFDirEntry);
	data = malloc(dirsize);
	if (data == NULL) {
		TIFFError(tif->tif_name,
		    "Cannot write directory, out of space");
		return (0);
	}
	/*
	 * Directory hasn't been placed yet, put
	 * it at the end of the file and link it
	 * into the existing directory structure.
	 */
	if (tif->tif_diroff == 0 && !TIFFLinkDirectory(tif))
		return (0);
	dataoff = tif->tif_diroff + 
		TIFFShortSize + TIFFDirSize*nfields + TIFFLongSize;
	if (dataoff & 1)
		dataoff++;
	(void) lseek(tif->tif_fd, dataoff, L_SET);
	tif->tif_curdir++;
	dir = (TIFFDirEntry *)data;
	/*
	 * Setup external form of directory
	 * entries and write data items.
	 */
	mybcopy(td->td_fieldsset, fields, sizeof (fields));
/*BEGIN XXX*/
	/*
	 * Write out ExtraSamples tag only if Matteing would
	 * be set to 1 (i.e. Associated Alpha data is present).
	 */
	if (FieldSet(fields, FIELD_MATTEING) && !td->td_matteing) {	/*XXX*/
		ResetFieldBit(fields, FIELD_MATTEING);			/*XXX*/
		nfields--;						/*XXX*/
		dirsize -= sizeof (TIFFDirEntry);			/*XXX*/
	}								/*XXX*/
/*END XXX*/
	for (fip = (TIFFFieldInfo *)tiffFieldInfo; fip->field_tag; fip++) {
		if (fip->field_bit == FIELD_IGNORE ||
		    !FieldSet(fields, fip->field_bit))
			continue;
		switch (fip->field_bit) {
		case FIELD_STRIPOFFSETS:
			/*
			 * We use one field bit for both strip and tile
			 * offsets, and so must be careful in selecting
			 * the appropriate field descriptor (so that tags
			 * are written in sorted order).
			 */
			tag = isTiled(tif) ?
			    TIFFTAG_TILEOFFSETS : TIFFTAG_STRIPOFFSETS;
			if (tag != fip->field_tag)
				continue;
			if (!TIFFWriteLongArray(tif, TIFF_LONG, tag, dir,
			    (int) td->td_nstrips, td->td_stripoffset))
				goto bad;
			break;
		case FIELD_STRIPBYTECOUNTS:
			/*
			 * We use one field bit for both strip and tile
			 * byte counts, and so must be careful in selecting
			 * the appropriate field descriptor (so that tags
			 * are written in sorted order).
			 */
			tag = isTiled(tif) ?
			    TIFFTAG_TILEBYTECOUNTS : TIFFTAG_STRIPBYTECOUNTS;
			if (tag != fip->field_tag)
				continue;
			if (!TIFFWriteLongArray(tif, TIFF_LONG, tag, dir,
			    (int) td->td_nstrips, td->td_stripbytecount))
				goto bad;
			break;
		case FIELD_COLORMAP:
			if (!TIFFWriteShortTable(tif, TIFFTAG_COLORMAP, dir,
			    3, td->td_colormap))
				goto bad;
			break;
		case FIELD_IMAGEDIMENSIONS:
			TIFFSetupShortLong(tif, TIFFTAG_IMAGEWIDTH,
			    dir++, td->td_imagewidth);
			TIFFSetupShortLong(tif, TIFFTAG_IMAGELENGTH,
			    dir, td->td_imagelength);
			break;
		case FIELD_TILEDIMENSIONS:
			TIFFSetupShortLong(tif, TIFFTAG_TILEWIDTH,
			    dir++, td->td_tilewidth);
			TIFFSetupShortLong(tif, TIFFTAG_TILELENGTH,
			    dir, td->td_tilelength);
			break;
		case FIELD_POSITION:
			WriteRationalPair(TIFF_RATIONAL,
			    TIFFTAG_XPOSITION, td->td_xposition,
			    TIFFTAG_YPOSITION, td->td_yposition);
			break;
		case FIELD_RESOLUTION:
			WriteRationalPair(TIFF_RATIONAL,
			    TIFFTAG_XRESOLUTION, td->td_xresolution,
			    TIFFTAG_YRESOLUTION, td->td_yresolution);
			break;
		case FIELD_BITSPERSAMPLE:
		case FIELD_MINSAMPLEVALUE:
		case FIELD_MAXSAMPLEVALUE:
		case FIELD_SAMPLEFORMAT:
			if (!TIFFWritePerSampleShorts(tif, fip->field_tag, dir))
				goto bad;
			break;
		case FIELD_PAGENUMBER:
		case FIELD_HALFTONEHINTS:
#ifdef YCBCR_SUPPORT
		case FIELD_YCBCRSUBSAMPLING:
#endif
#ifdef CMYK_SUPPORT
		case FIELD_DOTRANGE:
#endif
			TIFFSetupShortPair(tif, fip->field_tag, dir);
			break;
#ifdef JPEG_SUPPORT
		case FIELD_JPEGQTABLES:
			if (!TIFFWriteJPEGQTables(tif, dir))
				goto bad;
			break;
		case FIELD_JPEGDCTABLES:
			if (!TIFFWriteJPEGCTables(tif,
			    TIFFTAG_JPEGDCTABLES, dir, td->td_dctab))
				goto bad;
			break;
		case FIELD_JPEGACTABLES:
			if (!TIFFWriteJPEGCTables(tif,
			    TIFFTAG_JPEGACTABLES, dir, td->td_actab))
				goto bad;
			break;
#endif
#ifdef COLORIMETRY_SUPPORT
		case FIELD_REFBLACKWHITE:
			if (!TIFFWriteRationalArray(tif, TIFF_RATIONAL,
			    TIFFTAG_REFERENCEBLACKWHITE, dir,
			    2*td->td_samplesperpixel, td->td_refblackwhite))
				goto bad;
			break;
		case FIELD_TRANSFERFUNCTION:
			if (!TIFFWriteTransferFunction(tif, dir))
				goto bad;
			break;
#endif
		default:
			if (!TIFFWriteNormalTag(tif, dir, fip))
				goto bad;
			break;
		}
		dir++;
		ResetFieldBit(fields, fip->field_bit);
	}
	/*
	 * Write directory.
	 */
	(void) lseek(tif->tif_fd, tif->tif_diroff, L_SET);
	dircount = nfields;
	if (!TIFFWriteShort(tif->tif_fd, &dircount)) {
		TIFFError(tif->tif_name, "Error writing directory count");
		goto bad;
	}
	if (!TIFFWriteTIFFDir(tif->tif_fd, (TIFFDirEntry*)data, dircount)) {
		TIFFError(tif->tif_name, "Error writing directory contents");
		goto bad;
	}
	if (!WriteOK(tif->tif_fd, &tif->tif_nextdiroff, TIFFLongSize)) {
		TIFFError(tif->tif_name, "Error writing directory link");
		goto bad;
	}
	TIFFFreeDirectory(tif);
	free(data);
	tif->tif_flags &= ~TIFF_DIRTYDIRECT;

	/*
	 * Reset directory-related state for subsequent
	 * directories.
	 */
	TIFFDefaultDirectory(tif);
	tif->tif_diroff = 0;
	tif->tif_curoff = 0;
	tif->tif_row = -1;
	tif->tif_curstrip = -1;
	return (1);
bad:
	free(data);
	return (0);
}
#undef WriteRationalPair

static
DECLARE3(TIFFWriteTIFFDir,
    int, fd, TIFFDirEntry*, dir, int, n)
{
	int i;
	
	for (i=0; i< n; i++)
	{
		if(!TIFFWriteShort(fd,(short *)&dir->tdir_tag)) return (0);
		if(!TIFFWriteShort(fd,(short *)&dir->tdir_type)) return (0);
		if(!WriteOK(fd,&dir->tdir_count,TIFFLongSize)) return (0);
		if(!WriteOK(fd,&dir->tdir_offset,TIFFLongSize)) return (0);
		dir++;
	}
	return (1);
}


/*
 * Process tags that are not special cased.
 */
static
DECLARE3(TIFFWriteNormalTag,
    TIFF*, tif, TIFFDirEntry*, dir, TIFFFieldInfo*, fip)
{
	TIFFDirectory* td = &tif->tif_dir;
	u_short wc = (u_short) fip->field_writecount;

	dir->tdir_tag = fip->field_tag;
	dir->tdir_type = (u_short)fip->field_type;
	dir->tdir_count = wc;
#define	WRITE(x,y)	x(tif, fip->field_type, fip->field_tag, dir, wc, y)
	switch (fip->field_type) {
	case TIFF_SHORT:
	case TIFF_SSHORT:
		if (wc > 1) {
			u_short *wp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &wp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &wp);
			if (!WRITE(TIFFWriteShortArray, wp))
				return (0);
		} else {
			u_short sv;
			_TIFFgetfield(td, fip->field_tag, &sv);
			dir->tdir_offset =
			    TIFFInsertData(tif, dir->tdir_type, sv);
		}
		break;
	case TIFF_LONG:
	case TIFF_SLONG:
		if (wc > 1) {
			tiff_u_long *lp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &lp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &lp);
			if (!WRITE(TIFFWriteLongArray, lp))
				return (0);
		} else {
			/* XXX handle LONG->SHORT conversion */
			_TIFFgetfield(td, fip->field_tag, &dir->tdir_offset);
		}
		break;
	case TIFF_RATIONAL:
	case TIFF_SRATIONAL:
		if (wc > 1) {
			float *fp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &fp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &fp);
			if (!WRITE(TIFFWriteRationalArray, fp))
				return (0);
		} else {
			float fv;
			_TIFFgetfield(td, fip->field_tag, &fv);
			if (!TIFFWriteRational(tif, fip->field_type, fip->field_tag, dir, fv))
				return (0);
		}
		break;
	case TIFF_FLOAT:
		if (wc > 1) {
			float *fp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &fp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &fp);
			if (!WRITE(TIFFWriteFloatArray, fp))
				return (0);
		} else {
			float fv;
			_TIFFgetfield(td, fip->field_tag, &fv);
			TIFFCvtNativeToIEEEFloat(tif, 1, &fv);
			/* XXX assumes sizeof (long) == sizeof (float) */
			dir->tdir_offset = *(tiff_u_long *)&fv;	/* XXX */
		}
		break;
	case TIFF_ASCII: {
		char *cp;
		_TIFFgetfield(td, fip->field_tag, &cp);
		if (!TIFFWriteString(tif, fip->field_tag, dir, cp))
			return (0);
		break;
	}
	}
	return (1);
}
#undef WRITE

/*
 * Setup a directory entry with either a SHORT
 * or LONG type according to the value.
 */
static
DECLARE4(TIFFSetupShortLong,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, tiff_u_long, v)
{
	dir->tdir_tag = tag;
	dir->tdir_count = 1;
	if (v > 0xffffL) {
		dir->tdir_type = (short)TIFF_LONG;
		dir->tdir_offset = v;
	} else {
		dir->tdir_type = (short)TIFF_SHORT;
		dir->tdir_offset = TIFFInsertData(tif, (int)TIFF_SHORT, v);
	}
}
#undef MakeShortDirent

/*
 * Setup a RATIONAL directory entry and
 * write the associated indirect value.
 */
static
DECLARE5(TIFFWriteRational,
    TIFF*, tif, TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, float, v)
{
	tiff_u_long t[2];
 
	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = 1;
	if (type == TIFF_RATIONAL && (v < 0 || v > (tiff_u_long)ULONG_MAX))
		TIFFWarning(tif->tif_name,
	"\"%s\": Information lost writing value (%g) as (unsigned) RATIONAL",
		    TIFFFieldWithTag(tag)->field_name, v);
    TIFFFloatToRational(v,(tiff_u_long *)t);
	return (TIFFWriteData(tif, dir, (char *)t));
}

/*
 * Setup a directory entry that references a
 * samples/pixel array of SHORT values and
 * (potentially) write the associated indirect
 * values.
 */
static
DECLARE3(TIFFWritePerSampleShorts,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir)
{
	u_short w[4], v;
	int i, samplesperpixel = tif->tif_dir.td_samplesperpixel;

	_TIFFgetfield(&tif->tif_dir, tag, &v);
	for (i = 0; i < samplesperpixel; i++)
		w[i] = v;
	return (TIFFWriteShortArray(
	    tif, TIFF_SHORT, tag, dir, samplesperpixel, w));
}

/*
 * Setup a pair of shorts that are returned by
 * value, rather than as a reference to an array.
 */
static
DECLARE3(TIFFSetupShortPair,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir)
{
	u_short v[2];

	_TIFFgetfield(&tif->tif_dir, tag, &v[0], &v[1]);
	return (TIFFWriteShortArray(tif, TIFF_SHORT, tag, dir, 2, v));
}

/*
 * Setup a directory entry for an NxM table of shorts,
 * where M is known to be 2**bitspersample, and write
 * the associated indirect data.
 */
static
DECLARE5(TIFFWriteShortTable,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, int, n, u_short**, table)
{
	tiff_u_long off;
	int i;

	dir->tdir_tag = tag;
	dir->tdir_type = (short)TIFF_SHORT;
	/* XXX -- yech, fool TIFFWriteData */
	dir->tdir_count = 1L<<tif->tif_dir.td_bitspersample;
	off = dataoff;
	for (i = 0; i < n; i++)
	{
		TIFFPackShort( (short *)table[i], dir->tdir_count);
		if (!TIFFWriteData(tif, dir, (char *)table[i]))
			return (0);
		TIFFUnPackShort( (short *)table[i], dir->tdir_count);
	}
	dir->tdir_count *= n;
	dir->tdir_offset = off;
	return (1);
}

/*
 * Setup a directory entry of an ASCII string
 * and write any associated indirect value.
 */
static
DECLARE4(TIFFWriteString,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, char*, cp)
{
	dir->tdir_tag = tag;
	dir->tdir_type = (short)TIFF_ASCII;
	dir->tdir_count = strlen(cp) + 1;	/* includes \0 byte */
	if (dir->tdir_count > 4) {
		if (!TIFFWriteData(tif, dir, cp))
			return (0);
	} else
		mybcopy(cp, &dir->tdir_offset, dir->tdir_count);
	return (1);
}

/*
 * Setup a directory entry of an array of SHORT
 * or SSHORT and write the associated indirect values.
 */
static
DECLARE6(TIFFWriteShortArray, TIFF*, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, int, n, u_short*, v)
{
	int ok;

	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	if (n <= 2) {
		if (tif->tif_header.tiff_magic == TIFF_BIGENDIAN) {
			dir->tdir_offset = (long)v[0] << 16;
			if (n == 2)
				dir->tdir_offset |= v[1] & 0xffff;
		} else {
			dir->tdir_offset = v[0] & 0xffff;
			if (n == 2)
				dir->tdir_offset |= (long)v[1] << 16;
		}
		return (1);
	} else
	{
		TIFFPackShort((short *)v, n);
		ok = TIFFWriteData(tif, dir, (char *)v);
		TIFFUnPackShort( (short *)v, n);
		return (ok);
	}
}

/*
 * Setup a directory entry of an array of LONG
 * or SLONG and write the associated indirect values.
 */
static
DECLARE6(TIFFWriteLongArray, TIFF*, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, int, n, tiff_u_long*, v)
{
	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	if (n == 1) {
		dir->tdir_offset = v[0];
		return (1);
	} else
		return (TIFFWriteData(tif, dir, (char *)v));
}

/*
 * Setup a directory entry of an array of RATIONAL
 * or SRATIONAL and write the associated indirect values.
 */
static
DECLARE6(TIFFWriteRationalArray, TIFF*, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, int, n, float*, v)
{
	int i, status;
	tiff_u_long *t;

	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	t = (tiff_u_long *)malloc(2*n * sizeof (long));
	for (i = 0; i < n; i++) {
	    TIFFFloatToRational(v[i],t+2*i);
	}
	status = TIFFWriteData(tif, dir, (char *)t);
	free((char *)t);
	return (status);
}

static
DECLARE6(TIFFWriteFloatArray, TIFF *, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry *, dir, int, n, float *, v)
{
	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	TIFFCvtNativeToIEEEFloat(tif, n, v);
	if (n == 1) {
		dir->tdir_offset = *(tiff_u_long *)&v[0];
		return (1);
	} else
		return (TIFFWriteData(tif, dir, (char *)v));
}

#ifdef JPEG_SUPPORT
/*
 * Setup a directory entry for JPEG Quantization
 * tables and write the associated indirect values.
 */
static
DECLARE2(TIFFWriteJPEGQTables, TIFF*, tif, TIFFDirEntry*, dir)
{
	TIFFDirectory *td = &tif->tif_dir;
	TIFFDirEntry tdir;
	tiff_u_long off[4];
	int i;

	tdir.tdir_tag = TIFFTAG_JPEGQTABLES;	/* for diagnostics */
	tdir.tdir_type = (short)TIFF_BYTE;
	tdir.tdir_count = 64;
	for (i = 0; i < (int)td->td_samplesperpixel; i++) {
		if (!TIFFWriteData(tif, &tdir, (char *)td->td_qtab[i]))
			return (0);
		off[i] = tdir.tdir_offset;
	}
	return (TIFFWriteLongArray(tif, TIFF_LONG,
	    TIFFTAG_JPEGQTABLES, dir, td->td_samplesperpixel, off));
}

/*
 * Setup a directory entry for JPEG Coefficient
 * tables and write the associated indirect values.
 */
static
DECLARE4(TIFFWriteJPEGCTables,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, u_char **, tab)
{
	TIFFDirectory *td = &tif->tif_dir;
	TIFFDirEntry tdir;
	tiff_u_long off[4];
	int i, j, ncodes;

	tdir.tdir_tag = tag;		/* for diagnostics */
	tdir.tdir_type = (short)TIFF_BYTE;
	for (i = 0; i < (int)td->td_samplesperpixel; i++) {
		for (ncodes = 0, j = 0; j < 16; j++)
			ncodes += tab[i][j];
		tdir.tdir_count = 16+ncodes;
		if (!TIFFWriteData(tif, &tdir, (char *)tab[i]))
			return (0);
		off[i] = tdir.tdir_offset;
	}
	return (TIFFWriteLongArray(tif,
	    TIFF_LONG, tag, dir, td->td_samplesperpixel, off));
}
#endif

#ifdef COLORIMETRY_SUPPORT
static
DECLARE2(TIFFWriteTransferFunction, TIFF*, tif, TIFFDirEntry*, dir)
{
	TIFFDirectory *td = &tif->tif_dir;
	int j, ncols;
	tiff_u_long n;
	u_short **tf = td->td_transferfunction;

	/*
	 * Check if the table can be written as a single column.
	 */
	n = (1L<<td->td_bitspersample) * sizeof (u_short);
	ncols = 1;		/* assume only one column is needed */
	for (j = 1; j < (int)td->td_samplesperpixel; j++)
		if (mybcmp(tf[0], tf[j], n)) {
			ncols = td->td_samplesperpixel;
			break;
		}
	return (TIFFWriteShortTable(tif,
	    TIFFTAG_TRANSFERFUNCTION, dir, ncols, tf));
}
#endif

/*
 * Write a contiguous directory item.
 */
static
TIFFWriteData(tif, dir, cp)
	TIFF *tif;
	TIFFDirEntry *dir;
	char *cp;
{
	int cc;

	dir->tdir_offset = dataoff;
	cc = dir->tdir_count * tiffDataWidth[dir->tdir_type];
	if (SeekOK(tif->tif_fd, dir->tdir_offset) &&
	    WriteOK(tif->tif_fd, cp, cc)) {
		dataoff += (cc + 1) & ~1;
		return (1);
	}
	TIFFError(tif->tif_name, "Error writing data for field \"%s\"",
	    TIFFFieldWithTag(dir->tdir_tag)->field_name);
	return (0);
}

/*
 * Link the current directory into the
 * directory chain for the file.
 */
static
TIFFLinkDirectory(tif)
	register TIFF *tif;
{
	static char module[] = "TIFFLinkDirectory";
	u_short dircount;
	long nextdir;

	tif->tif_diroff = (lseek(tif->tif_fd, 0L, L_XTND)+1) &~ 1L;
	if (tif->tif_header.tiff_diroff == 0) {
		/*
		 * First directory, overwrite header.
		 */
		tif->tif_header.tiff_diroff = tif->tif_diroff;
		(void) lseek(tif->tif_fd, 0L, L_SET);
		if (!TIFFWriteTIFFHdr(tif->tif_fd, &tif->tif_header)) {
			TIFFError(tif->tif_name, "Error writing TIFF header");
			return (0);
		}
		return (1);
	}
	/*
	 * Not the first directory, search to the last and append.
	 */
	nextdir = tif->tif_header.tiff_diroff;
	do {
		if (!SeekOK(tif->tif_fd, nextdir) ||
		    !TIFFReadShort(tif->tif_fd, (short *)&dircount)) {
			TIFFError(module, "Error fetching directory count");
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabShort(&dircount);
		lseek(tif->tif_fd, dircount * TIFFDirSize, L_INCR);
		if (!ReadOK(tif->tif_fd, &nextdir, TIFFLongSize)) {
			TIFFError(module, "Error fetching directory link");
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabLong((tiff_u_long *)&nextdir);
	} while (nextdir != 0);
	(void) lseek(tif->tif_fd, -TIFFLongSize, L_INCR);
	if (!WriteOK(tif->tif_fd, &tif->tif_diroff, TIFFLongSize)) {
		TIFFError(module, "Error writing directory link");
		return (0);
	}
	return (1);
}

#ifdef CARTOGRAPHIC_SUPPORT
/*
 * Write the contents of the cartographic directory
 * to the specified file.
 */
CARTWriteDirectory(tif)
	TIFF *tif;
{
	u_short dircount;
	int nfields, dirsize;
	char *data;
	TIFFFieldInfo *fip;
	TIFFDirEntry *dir;
	TIFFDirectory *td;
	long nextoff=0L;

	if (tif->tif_mode == O_RDONLY)
		return (1);

	td = &tif->tif_dir;
	/*
	 * Size the directory so that we can calculate
	 * offsets for the data items that aren't kept
	 * in-place in each field. All cart fields counted.
	 */
	nfields = 0;
	for (fip = (TIFFFieldInfo *)cartFieldInfo; fip->field_tag; fip++)
		nfields++;
	dirsize = nfields * sizeof (TIFFDirEntry);
	data = malloc(dirsize);
	if (data == NULL) {
		TIFFError(tif->tif_name,
		    "Cannot write CART directory, out of space");
		return (0);
	}
	/*
	 * If Directory hasn't been placed yet, put
	 * it at the end of the file and link it
	 * into the existing directory structure.
	 */
	if (td->td_carto_ifd_offset == 0 && !CARTLinkDirectory(tif))
		return (0);
	/* here's the offset to data written outside of tag */
	dataoff = td->td_carto_ifd_offset + 
		TIFFShortSize + TIFFDirSize*nfields + TIFFLongSize;
	if (dataoff & 1)
		dataoff++;
	(void) lseek(tif->tif_fd, dataoff, L_SET);
	dir = (TIFFDirEntry *)data;

	/*
	 * Setup external form of carto directory
	 * entries and write data items.
	 * Field-bit stuff not yet needed; All fields
	 * should be written out.
	 */
	for (fip = (TIFFFieldInfo *)cartFieldInfo; fip->field_tag; fip++) {
		if (!TIFFWriteNormalTag(tif, dir, fip))
			goto bad;
		dir++;
	}
	/*
	 * Write private directory.
	 */
	(void) lseek(tif->tif_fd, td->td_carto_ifd_offset, L_SET);
	dircount = nfields;
	if (!TIFFWriteShort(tif->tif_fd, (short *)&dircount)) {
		TIFFError(tif->tif_name, "Error writing CART directory count");
		goto bad;
	}
	if (!TIFFWriteTIFFDir(tif->tif_fd, (TIFFDirEntry*)data, dircount)) {
		TIFFError(tif->tif_name, "Error writing CART directory contents");
		goto bad;
	}
	if (!WriteOK(tif->tif_fd, &nextoff, TIFFLongSize)) {
		TIFFError(tif->tif_name, "Error writing CART directory link");
		goto bad;
	}

	free(data);
	tif->tif_flags &= ~TIFF_DIRTYCART;

	return (1);
bad:
	free(data);
	return (0);
}

/*
 * Link the current CART directory into the
 * current directory's carto_ifd offset value..
 */
static
CARTLinkDirectory(tif)
	register TIFF *tif;
{
	tiff_u_long offset;
	
	if (!(offset = (lseek(tif->tif_fd, 0L, L_XTND)+1)))
		return(0);
	offset = offset & ~1L;   /* must be on word boundary */
	TIFFSetField(tif, TIFFTAG_CARTO_IFD_OFFSET, offset);
	return (1);
}
#endif /* CARTO */

/*
 * Convert a floating point value to rational pair
 * using the Continued Fraction Expansion of the value.
 * This gives a fraction accurate to seven significant digits.
 */
static
DECLARE2(TIFFFloatToRational,float,in,tiff_u_long *,outr)
{
	dblparam_t threshold;

	threshold=(tiff_u_long)ULONG_MAX;
	threshold=1.1/threshold;

	if (in>=(1/threshold)) in= (0.999/threshold);
	if (in < 1.0)
		TIFFgetfraction((dblparam_t)in,outr,outr+1,threshold,0);
	else
		TIFFgetfraction((dblparam_t)1/in,outr+1,outr,threshold,0);
}

/* Recursive Computation of Continued Fraction (x <= 1) */
#define RECURSION_LIMIT 15
static
TIFFgetfraction(x, num, den, thresh,level)
dblparam_t x;
tiff_u_long *num;
tiff_u_long *den;
dblparam_t thresh;
int level;
{
	tiff_u_long k,num1,den1;
	dblparam_t rem;

	if (x <= thresh || ++level > RECURSION_LIMIT ) /* We're done */
	{
		*num=0;
		*den=1;
	}
	else /* get fraction of remainder */
	{
		k=1/x;
		rem=(1/x)-k;
		TIFFgetfraction(rem, &num1, &den1, thresh*(k+1),level);
		*num = den1;
		*den = k*den1 + num1;
	}
}

/* end dir_writ */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_dirr.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * Directory Tag Get & Set Routines.
 * (and also some miscellaneous stuff)
 *
 * NB: Beware of the varargs declarations for routines in
 *     this file.  The names and types of variables has been
 *     carefully chosen to make things work with compilers that
 *     are busted in one way or another (e.g. SGI/MIPS).
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <strings.h>

/* from dir_read */

#define	IGNORE	0		/* tag placeholder used below */

#if HAVE_IEEEFP
#define	TIFFCvtIEEEFloatToNative(tif, n, fp)
#endif

#include "prototypes.h"
#if USE_PROTOTYPES
static	EstimateStripByteCounts(TIFF *, TIFFDirEntry *, u_int);
static	MissingRequired(TIFF *, char *);
static	CheckDirCount(TIFF *, TIFFDirEntry *, tiff_u_long);
static	TIFFFetchData(TIFF *, TIFFDirEntry *, char *);
static	TIFFFetchString(TIFF *, TIFFDirEntry *, char *);
static	float TIFFFetchRational(TIFF *, TIFFDirEntry *);
static	TIFFFetchNormalTag(TIFF *, TIFFDirEntry *);
static	TIFFFetchLongShortTag(TIFF *, TIFFDirEntry *);
static	TIFFFetchPerSampleShorts(TIFF *, TIFFDirEntry *, long *);
static	TIFFFetchShortArray(TIFF *, TIFFDirEntry *, u_short []);
static	TIFFFetchStripThing(TIFF *, TIFFDirEntry *, long, tiff_u_long **);
static	TIFFFetchRefBlackWhite(TIFF *, TIFFDirEntry *);
static	TIFFFetchJPEGQTables(TIFF *, TIFFDirEntry *);
static	TIFFFetchJPEGCTables(TIFF *, TIFFDirEntry *, u_char ***);
static	TIFFFetchExtraSamples(TIFF *, TIFFDirEntry *);
static	float TIFFFetchFloat(TIFF *, TIFFDirEntry *);
static	int TIFFFetchFloatArray(TIFF *, TIFFDirEntry *, float *);
static  TIFFDirEntry* TIFFGetDirectoryEntries(TIFF *,tiff_u_long,u_short *,long *);
extern	int TIFFSetCompressionScheme(TIFF *, int);
extern	int TIFFDefaultDirectory(TIFF*);
extern	int TIFFFreeDirectory(TIFF*);
static  void CARTReadDirectory(TIFF*);
static  void CARTReadOldDirectory(TIFF*);
static  int TIFFReadTIFFDir(int fd, TIFFDirEntry* dir, int n);
#else
static	EstimateStripByteCounts();
static	MissingRequired();
static	CheckDirCount();
static	TIFFFetchData();
static	TIFFFetchString();
static	float TIFFFetchRational();
static	TIFFFetchNormalTag();
static	TIFFFetchLongShortTag();
static	TIFFFetchPerSampleShorts();
static	TIFFFetchShortArray();
static	TIFFFetchStripThing();
static	TIFFFetchRefBlackWhite();
static	TIFFFetchJPEGQTables();
static	TIFFFetchJPEGCTables();
static	TIFFFetchExtraSamples();
static	float TIFFFetchFloat();
static	int TIFFFetchFloatArray();
static  TIFFDirEntry* TIFFGetDirectoryEntries();
extern	int TIFFSetCompressionScheme();
extern	int TIFFDefaultDirectory();
extern	int TIFFFreeDirectory();
static  void CARTReadDirectory();
static  void CARTReadOldDirectory();
static  int TIFFReadTIFFDir();
#endif

/* start dir_read */

#ifdef vms

#if USE_PROTOTYPES
tiff_u_long GetLong(char **ptr);
#else
tiff_u_long GetLong();
#endif

tiff_u_long GetLong( ptr )
char **ptr;
{
	tiff_u_long value;
	long v1,v2;
	int length;

	length = strspn(*ptr,"0123456789");
	if (length < 10)
	{
		sscanf(*ptr, "%ld",&v1);
		value = v1;
	}
	else /* length==10 */
	{
		sscanf(*ptr,"%1ld%ld",&v1,&v2);
		value = 1000000000U*(tiff_u_long)v1 + (tiff_u_long)v2;
	}

	*ptr += length+1;
	return (value);
}
#endif

static char *
CheckMalloc(tif, n, what)
	TIFF *tif;
	int n;
	char *what;
{
	char *cp = malloc(n);
	if (cp == NULL)
		TIFFError(tif->tif_name, "No space %s", what);
	return (cp);
}

#define STATUS_IGNORE -1L
#define STATUS_NOTFOUND -2L
#define STATUS_UNKNOWN -3L
/*
 * Find the field information entry for this tag.
 * and give the offset from the base, or a 
 * negative error status.
 */
static long 
TIFFDirFieldOffset(tif,dp,base,start)
TIFF *tif;
TIFFDirEntry *dp;
TIFFFieldInfo *base;
TIFFFieldInfo *start;
{
	register TIFFFieldInfo *fip=start;
	int tag=dp->tdir_tag;
	int type=dp->tdir_type;
	static int diroutoforderwarning=0;

	/*
	 * Silicon Beach (at least) writes unordered
	 * directory tags (violating the spec).  Handle
	 * it here, but be obnoxious (maybe they'll fix it?).
	 */
	if (tag < (int)fip->field_tag) {
		if (!diroutoforderwarning) {
			TIFFWarning(tif->tif_name,
	"invalid TIFF directory; tags are not sorted in ascending order");
			diroutoforderwarning = 1;
		}
		fip = base;
	}
	while (fip->field_tag && (int)fip->field_tag < tag)
		fip++;
	if (!fip->field_tag || fip->field_tag != tag) {
		TIFFWarning(tif->tif_name,
		    "unknown TIFF field with tag %d (0x%x) ignored",
		    tag,  tag);
		return(STATUS_UNKNOWN); /* set tag IGNORE & reset fip */
	}
	/*
	 * This is an old tag that we ignore. set tag IGNORE, dont reset.
	 */
	if (fip->field_bit == FIELD_IGNORE)
		return(STATUS_IGNORE);
	/*
	 * Check data type. If wrong, also IGNORE.
	 */
	while (type != (u_short)fip->field_type) {
		if (fip->field_type == TIFF_ANY)	/* wildcard */
			break;
		fip++;
		if (!fip->field_tag || fip->field_tag != tag) {
			TIFFWarning(tif->tif_name,
			   "wrong data type %d for \"%s\"; tag ignored",
			    dp->tdir_type, fip[-1].field_name);
			return(STATUS_IGNORE);
		}
	}
	/*
	 * Check count if known in advance. If not expected, IGNORE.
	 */
	if (fip->field_readcount != TIFF_VARIABLE) {
		tiff_u_long expected = (fip->field_readcount == TIFF_SPP) ?
		    (tiff_u_long) tif->tif_dir.td_samplesperpixel :
		    (tiff_u_long) fip->field_readcount;
		if (!CheckDirCount(tif, dp, expected))
			return(STATUS_IGNORE); /* set tag IGNORE, dont reset */
	}
	return((long)(fip-base));
}


/*
 * Go to offset and read the next TIFF directory
 * entries from a file. Also return number of entries
 * found and offset to next entry.
 */
static TIFFDirEntry*
TIFFGetDirectoryEntries(tif,offset,dcount,nextoff)
	TIFF *tif;
	tiff_u_long offset;
	u_short *dcount;
	long *nextoff;  /* NULL if none desired */
{
	register TIFFDirEntry* dir=NULL;
	u_short dircount;
	int i;
	
	if (!isMapped(tif)) {
		if (!SeekOK(tif->tif_fd, offset)) {
			TIFFError(tif->tif_name,
			    "Seek error accessing TIFF directory");
			goto bad;
		}
		if (!TIFFReadShort(tif->tif_fd, (short *)&dircount)) {
			TIFFError(tif->tif_name,
			    "Can not read TIFF directory count");
			goto bad;
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabShort(&dircount);
		dir = (TIFFDirEntry *)CheckMalloc(tif,
		    dircount * sizeof (TIFFDirEntry), "to read TIFF directory");
		if (dir == NULL)
			goto bad;
		if (!TIFFReadTIFFDir(tif->tif_fd, dir, dircount)) {
			TIFFError(tif->tif_name, "Can not read TIFF directory");
			goto bad;
		}
		/*
		 * Read offset to next directory for sequential scans.
		 */
		if (nextoff &&
			(!ReadOK(tif->tif_fd, &tif->tif_nextdiroff, TIFFLongSize)))
			tif->tif_nextdiroff = 0;
#ifdef MMAP_SUPPORT
	} else {
		off_t off = tif->tif_diroff;

		if (off + TIFFShortSize > tif->tif_size) {
			TIFFError(tif->tif_name,
			    "Can not read TIFF directory count");
			goto bad;
		} else
			mybcopy(tif->tif_base + off, &dircount, sizeof (short));
		off += sizeof (short);
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabShort(&dircount);
		dir = (TIFFDirEntry *)CheckMalloc(tif,
		    dircount * sizeof (TIFFDirEntry), "to read TIFF directory");
		if (dir == NULL)
			return (0);
		if (off + dircount*sizeof (TIFFDirEntry) > tif->tif_size) {
			TIFFError(tif->tif_name, "Can not read TIFF directory");
			goto bad;
		} else
			mybcopy(tif->tif_base + off, dir,
			    dircount*sizeof (TIFFDirEntry));
		off += dircount* sizeof (TIFFDirEntry);
		if (nextoff)
		{
			if (off + sizeof (long) < tif->tif_size)
				mybcopy(tif->tif_base + off, nextoff,
				    sizeof (long));
			else
				*nextoff = 0;
		}
#endif
	}
	/*
	 *  Swap the file values, if needed 
	 */
	if (tif->tif_flags & TIFF_SWAB) {
		for (i=0;i<(int)dircount;i++)
		{
			TIFFSwabArrayOfShort(&dir[i].tdir_tag, 2);
			TIFFSwabArrayOfLong(&dir[i].tdir_count, 2);
		}
		if (nextoff) TIFFSwabLong((tiff_u_long *)nextoff);
	}
		
	*dcount=dircount;
	return (dir);
bad:
	if (dir)
		free( (char *)dir);
	return (0);
}

/*
 * Read the next TIFF directory from a file
 * and convert it to the internal format.
 * We read directories sequentially.
 */
TIFFReadDirectory(tif)
	TIFF *tif;
{
	register TIFFFieldInfo *fip;
	register TIFFDirEntry *dp;
	register TIFFDirectory *td;
	register int n;
	TIFFDirEntry *dir;
	long v;
	u_short dircount;
	char *cp;
	long off;

	tif->tif_diroff = tif->tif_nextdiroff;
	if (tif->tif_diroff == 0)		/* no more directories */
		return (0);
	tif->tif_curdir++;

	dir = TIFFGetDirectoryEntries(tif,tif->tif_diroff,
		&dircount,&tif->tif_nextdiroff)	;

	tif->tif_flags &= ~(TIFF_BEENWRITING|TIFF_ISTILED);/* reset before new dir */
#ifdef CARTOGRAPHIC_SUPPORT
	tif->tif_flags &= ~TIFF_HASCARTTAGS;
#endif
	/*
	 * Setup default value and then make a pass over
	 * the fields to check type and tag information,
	 * and to extract info required to size data
	 * structures.  A second pass is made afterwards
	 * to read in everthing not taken in the first pass.
	 */
	td = &tif->tif_dir;
	/* free any old stuff and reinit */
	TIFFFreeDirectory(tif);
	TIFFDefaultDirectory(tif);
	/*
	 * Electronic Arts writes gray-scale TIFF files
	 * without a PlanarConfiguration directory entry.
	 * Thus we setup a default value here, even though
	 * the TIFF spec says there is no default value.
	 */
	TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	for (fip = (TIFFFieldInfo *)tiffFieldInfo, dp = dir, n = dircount; n > 0; n--, dp++) {

		/*
		 * Find the field information entry for this tag.
		 */
		off = TIFFDirFieldOffset(tif,dp,(TIFFFieldInfo *)tiffFieldInfo,fip);
		if (off >= 0)
			fip=(TIFFFieldInfo *)tiffFieldInfo+off;
		else switch (off)
		{
			case STATUS_UNKNOWN:
				fip = (TIFFFieldInfo *)tiffFieldInfo; /* restart search */
				/* fall through to.. */
			case STATUS_IGNORE:
				dp->tdir_tag = IGNORE;
				continue;
		}
		
		switch (dp->tdir_tag) {
		case TIFFTAG_STRIPOFFSETS:
		case TIFFTAG_STRIPBYTECOUNTS:
		case TIFFTAG_TILEOFFSETS:
		case TIFFTAG_TILEBYTECOUNTS:
			TIFFSetFieldBit(tif, fip->field_bit);
			break;
		case TIFFTAG_PLANARCONFIG:
		case TIFFTAG_SAMPLESPERPIXEL:
			if (!TIFFFetchNormalTag(tif, dp))
				goto bad;
			break;
		case TIFFTAG_ROWSPERSTRIP:
		case TIFFTAG_IMAGEWIDTH:
		case TIFFTAG_IMAGELENGTH:
		case TIFFTAG_IMAGEDEPTH:
		case TIFFTAG_TILELENGTH:
		case TIFFTAG_TILEWIDTH:
		case TIFFTAG_TILEDEPTH:
			if (!TIFFFetchLongShortTag(tif, dp))
				goto bad;
			break;
		}
	}

	/*
	 * Allocate directory structure and setup defaults.
	 */
	if (!TIFFFieldSet(tif, FIELD_IMAGEDIMENSIONS)) {
		MissingRequired(tif, "ImageLength");
		goto bad;
	}
	if (!TIFFFieldSet(tif, FIELD_PLANARCONFIG)) {
		MissingRequired(tif, "PlanarConfiguration");
		goto bad;
	}
	/* 
 	 * Setup appropriate structures (by strip or by tile)
	 */
	if (!TIFFFieldSet(tif, FIELD_TILEDIMENSIONS)) {
		td->td_stripsperimage = (td->td_rowsperstrip == 0xffffffff ?
		     (td->td_imagelength != 0 ? 1 : 0) :
		     howmany(td->td_imagelength, td->td_rowsperstrip));
		td->td_tilewidth = td->td_imagewidth;
		td->td_tilelength = td->td_rowsperstrip;
		td->td_tiledepth = td->td_imagedepth;
		tif->tif_flags &= ~TIFF_ISTILED;
	} else {
		td->td_stripsperimage = TIFFNumberOfTiles(tif);
		tif->tif_flags |= TIFF_ISTILED;
	}
	td->td_nstrips = td->td_stripsperimage;
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE)
		td->td_nstrips *= td->td_samplesperpixel;
	if (td->td_nstrips > 0 && !TIFFFieldSet(tif, FIELD_STRIPOFFSETS)) {
		MissingRequired(tif,
		    isTiled(tif) ? "TileOffsets" : "StripOffsets");
	/*	goto bad;  ndr commented out - allow non-images */
	}

	/*
	 * Second pass: extract other information.
	 */
	for (dp = dir, n = dircount; n > 0; n--, dp++) {
		if (dp->tdir_tag == IGNORE)
			continue;
		switch (dp->tdir_tag) {
		case TIFFTAG_COMPRESSION:
		case TIFFTAG_MINSAMPLEVALUE:
		case TIFFTAG_MAXSAMPLEVALUE:
		case TIFFTAG_BITSPERSAMPLE:
			/*
			 * The 5.0 spec says the Compression tag has
			 * one value, while earlier specs say it has
			 * one value per sample.  Because of this, we
			 * accept the tag if one value is supplied.
			 *
			 * The MinSampleValue, MaxSampleValue and
			 * BitsPerSample tags are supposed to be written
			 * as one value/sample, but some vendors incorrectly
			 * write one value only -- so we accept that
			 * as well (yech).
			 */
			if (dp->tdir_count == 1) {
				v = TIFFExtractData(tif,
				    dp->tdir_type, dp->tdir_offset);
				if (!TIFFSetField(tif, dp->tdir_tag, (int)v))
					goto bad;
				break;
			}
			/* fall thru... */
		case TIFFTAG_DATATYPE:
		case TIFFTAG_SAMPLEFORMAT:
			if (!TIFFFetchPerSampleShorts(tif, dp, &v) ||
			    !TIFFSetField(tif, dp->tdir_tag, (int)v))
				goto bad;
			break;
		case TIFFTAG_STRIPOFFSETS:
		case TIFFTAG_TILEOFFSETS:
			if (!TIFFFetchStripThing(tif, dp,
			    td->td_nstrips, &td->td_stripoffset))
				goto bad;
			break;
		case TIFFTAG_STRIPBYTECOUNTS:
		case TIFFTAG_TILEBYTECOUNTS:
			if (!TIFFFetchStripThing(tif, dp,
			    td->td_nstrips, &td->td_stripbytecount))
				goto bad;
			break;
		case TIFFTAG_IMAGELENGTH:
		case TIFFTAG_IMAGEWIDTH:
		case TIFFTAG_IMAGEDEPTH:
		case TIFFTAG_ROWSPERSTRIP:
		case TIFFTAG_TILELENGTH:
		case TIFFTAG_TILEWIDTH:
		case TIFFTAG_TILEDEPTH:
		case TIFFTAG_SAMPLESPERPIXEL:
		case TIFFTAG_PLANARCONFIG:
			/* handled in first pass above */
			break;
		case TIFFTAG_COLORMAP:
			if (!CheckDirCount(tif,dp,3*(1L<<td->td_bitspersample)))
				break;
			/* fall thru... */
		case TIFFTAG_TRANSFERFUNCTION:
			v = (1L<<td->td_bitspersample) * sizeof (u_short);
			cp = CheckMalloc(tif,
			    dp->tdir_count * sizeof (u_short),
			    "to read \"TransferFunction\" tag");
			if (cp != NULL) {
				if (TIFFFetchData(tif, dp, cp)) {
				
					TIFFUnPackShort((short*)cp, dp->tdir_count );
					
					/*
					 * This deals with there being only
					 * one array to apply to all samples.
					 */
					if (dp->tdir_count == 1L<<td->td_bitspersample)
						v = 0;
					/* NB: we assume samples/pixel <= 4 */
					TIFFSetField(tif, dp->tdir_tag,
					    cp, cp+v, cp+2*v, cp+3*v);
				}
				free(cp);
			}
			break;
		case TIFFTAG_PAGENUMBER:
			if (TIFFFetchShortArray(tif, dp, td->td_pagenumber))
				TIFFSetFieldBit(tif, FIELD_PAGENUMBER);
			break;
		case TIFFTAG_HALFTONEHINTS:
			if (TIFFFetchShortArray(tif, dp, td->td_halftonehints))
				TIFFSetFieldBit(tif, FIELD_HALFTONEHINTS);
			break;
#ifdef COLORIMETRY_SUPPORT
		case TIFFTAG_REFERENCEBLACKWHITE:
			(void) TIFFFetchRefBlackWhite(tif, dp);
			break;
#endif
#ifdef YCBCR_SUPPORT
		case TIFFTAG_YCBCRSUBSAMPLING:
			if (TIFFFetchShortArray(tif, dp, td->td_ycbcrsubsampling))
				TIFFSetFieldBit(tif, FIELD_YCBCRSUBSAMPLING);
			break;
#endif
#ifdef CMYK_SUPPORT
		case TIFFTAG_DOTRANGE:
			if (TIFFFetchShortArray(tif, dp, td->td_dotrange))
				TIFFSetFieldBit(tif, FIELD_DOTRANGE);
			break;
#endif
#ifdef JPEG_SUPPORT
		case TIFFTAG_JPEGQTABLES:
			if (TIFFFetchJPEGQTables(tif, dp))
				TIFFSetFieldBit(tif, FIELD_JPEGQTABLES);
			break;
		case TIFFTAG_JPEGDCTABLES:
			if (TIFFFetchJPEGCTables(tif, dp, &td->td_dctab))
				TIFFSetFieldBit(tif, FIELD_JPEGDCTABLES);
			break;
		case TIFFTAG_JPEGACTABLES:
			if (TIFFFetchJPEGCTables(tif, dp, &td->td_actab))
				TIFFSetFieldBit(tif, FIELD_JPEGACTABLES);
			break;
#endif
		case TIFFTAG_EXTRASAMPLES:
			(void) TIFFFetchExtraSamples(tif, dp);
			break;
/* BEGIN REV 4.0 COMPATIBILITY */
		case TIFFTAG_OSUBFILETYPE:
			v = 0;
			switch (TIFFExtractData(tif, dp->tdir_type,
			    dp->tdir_offset)) {
			case OFILETYPE_REDUCEDIMAGE:
				v = FILETYPE_REDUCEDIMAGE;
				break;
			case OFILETYPE_PAGE:
				v = FILETYPE_PAGE;
				break;
			}
			if (v)
				(void) TIFFSetField(tif,
				    TIFFTAG_SUBFILETYPE, (int)v);
			break;
/* END REV 4.0 COMPATIBILITY */
		default:
			(void) TIFFFetchNormalTag(tif, dp);
			break;
		}
	}
	/*
	 * Verify Palette image has a Colormap.
	 */
	if (td->td_photometric == PHOTOMETRIC_PALETTE &&
	    !TIFFFieldSet(tif, FIELD_COLORMAP)) {
		MissingRequired(tif, "Colormap");
		goto bad;
	}
	/*
	 * Attempt to deal with a missing StripByteCounts tag.
	 */
	if (!TIFFFieldSet(tif, FIELD_STRIPBYTECOUNTS)) {
		/*
		 * Some manufacturers violate the spec by not giving
		 * the size of the strips.  In this case, assume there
		 * is one uncompressed strip of data.
		 */
		if (td->td_nstrips > 1) {
		    MissingRequired(tif, "StripByteCounts");
		/*    goto bad; ndr commented out - allows null image */
		}
		TIFFWarning(tif->tif_name,
"TIFF directory is missing required \"%s\" field, calculating from imagelength",
		    TIFFFieldWithTag(TIFFTAG_STRIPBYTECOUNTS)->field_name);
		EstimateStripByteCounts(tif, dir, dircount);
	} else if (td->td_nstrips == 1 && td->td_stripbytecount[0] == 0) {
		/*
		 * Plexus (and others) sometimes give a value
		 * of zero for a tag when they don't know what
		 * the correct value is!  Try and handle the
		 * simple case of estimating the size of a one
		 * strip image.
		 */
		TIFFWarning(tif->tif_name,
"Bogus \"%s\" field, ignoring and calculating from imagelength",
		    TIFFFieldWithTag(TIFFTAG_STRIPBYTECOUNTS)->field_name);
		EstimateStripByteCounts(tif, dir, dircount);
	}
	if (dir)
		free((char *)dir);
	if (!TIFFFieldSet(tif, FIELD_MAXSAMPLEVALUE))
		td->td_maxsamplevalue = (1L<<td->td_bitspersample)-1;
	/*
	 * Setup default compression scheme.
	 */
	if (!TIFFFieldSet(tif, FIELD_COMPRESSION))
		TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);

#ifdef CARTOGRAPHIC_SUPPORT
	if (TIFFFieldSet(tif, FIELD_CARTO_IFD_OFFSET))
		CARTReadDirectory(tif);
	else if (TIFFFieldSet(tif, FIELD_IMAGEDESCRIPTION))
		CARTReadOldDirectory(tif);
#endif

	/*
	 * Reinitialize i/o since we are starting on a new directory.
	 */
	tif->tif_row = -1;
	tif->tif_curstrip = -1;
	tif->tif_col = -1;
	tif->tif_curtile = -1;
	tif->tif_tilesize = TIFFTileSize(tif);
	tif->tif_scanlinesize = TIFFScanlineSize(tif);
	return (1);
bad:
	if (dir)
		free((char *)dir);
	return (0);
}

static
EstimateStripByteCounts(tif, dir, dircount)
	TIFF *tif;
	TIFFDirEntry *dir;
	u_int dircount;
{
	register TIFFDirEntry *dp;
	register TIFFDirectory *td = &tif->tif_dir;
	register int n;

	td->td_stripbytecount = (tiff_u_long *)
	    CheckMalloc(tif, sizeof (tiff_u_long), "for \"StripByteCounts\" array");
	if (td->td_compression != COMPRESSION_NONE) {
		tiff_u_long space = TIFFHdrSize
		    + TIFFShortSize
		    + (dircount * TIFFDirSize)
		    + TIFFLongSize;
		long filesize = TIFFGetFileSize(tif->tif_fd);
		/* calculate amount of space used by indirect values */
		for (dp = dir, n = dircount; n > 0; n--, dp++) {
			int cc = dp->tdir_count * tiffDataWidth[dp->tdir_type];
			if (cc > sizeof (long))
				space += cc;
		}
		td->td_stripbytecount[0] = filesize - space;
		/*
		 * This gross hack handles the case where the offset to
		 * the strip is past the place where we think the strip
		 * should begin.  Since a strip of data must be contiguous,
		 * it's safe to assume that we've overestimated the amount
		 * of data in the strip and trim this number back accordingly.
		 */ 
		if (td->td_stripoffset[0] + td->td_stripbytecount[0] > filesize)
			td->td_stripbytecount[0] =
			    filesize - td->td_stripoffset[0];
	} else {
		tiff_u_long rowbytes = howmany(td->td_bitspersample *
		    td->td_samplesperpixel * td->td_imagewidth, 8);
		td->td_stripbytecount[0] = td->td_imagelength * rowbytes;
	}
	TIFFSetFieldBit(tif, FIELD_STRIPBYTECOUNTS);
	if (!TIFFFieldSet(tif, FIELD_ROWSPERSTRIP))
		td->td_rowsperstrip = td->td_imagelength;
}

static
MissingRequired(tif, tagname)
	TIFF *tif;
	char *tagname;
{
	TIFFError(tif->tif_name,
	    "TIFF directory is missing required \"%s\" field", tagname);
}

static
DECLARE3(TIFFReadTIFFDir,
    int, fd, TIFFDirEntry*, dir, int, n)
{
	int i;
	
	for (i=0; i< n; i++)
	{
		if(!TIFFReadShort(fd,(short *)&dir->tdir_tag)) return (0);
		if(!TIFFReadShort(fd,(short *)&dir->tdir_type)) return (0);
		if(!ReadOK(fd,&dir->tdir_count,TIFFLongSize)) return (0);
		if(!ReadOK(fd,&dir->tdir_offset,TIFFLongSize)) return (0);
		dir++;
	}
	return (1);
}




/*
 * Check the count field of a directory
 * entry against a known value.  The caller
 * is expected to skip/ignore the tag if
 * there is a mismatch.
 */
static
CheckDirCount(tif, dir, count)
	TIFF *tif;
	TIFFDirEntry *dir;
	tiff_u_long count;
{
	if (count != dir->tdir_count) {
		TIFFWarning(tif->tif_name,
	"incorrect count for field \"%s\" (%lu, expecting %lu); tag ignored",
		    TIFFFieldWithTag(dir->tdir_tag)->field_name,
		    dir->tdir_count, count);
		return (0);
	}
	return (1);
}

/*
 * Fetch a contiguous directory item.
 */
static
TIFFFetchData(tif, dir, cp)
	TIFF *tif;
	TIFFDirEntry *dir;
	char *cp;
{
	int cc, w;

	w = tiffDataWidth[dir->tdir_type];
	cc = dir->tdir_count * w;
	if (!isMapped(tif)) {
		if (!SeekOK(tif->tif_fd, dir->tdir_offset))
			goto bad;
		if (!ReadOK(tif->tif_fd, cp, cc))
			goto bad;
#ifdef MMAP_SUPPORT
	} else {
		if (dir->tdir_offset + cc > tif->tif_size)
			goto bad;
		mybcopy(tif->tif_base + dir->tdir_offset, cp, cc);
#endif
	}
	if (tif->tif_flags & TIFF_SWAB) {
		switch (dir->tdir_type) {
		case TIFF_SHORT:
		case TIFF_SSHORT:
			TIFFSwabArrayOfShort((u_short *)cp, dir->tdir_count);
			break;
		case TIFF_LONG:
		case TIFF_SLONG:
		case TIFF_FLOAT:
			TIFFSwabArrayOfLong((tiff_u_long *)cp, dir->tdir_count);
			break;
		case TIFF_RATIONAL:
		case TIFF_SRATIONAL:
			TIFFSwabArrayOfLong((tiff_u_long *)cp, 2*dir->tdir_count);
			break;
		}
	}
	return (cc);
bad:
	TIFFError(tif->tif_name, "Error fetching data for field \"%s\"",
	    TIFFFieldWithTag(dir->tdir_tag)->field_name);
	return (0);
}

#ifdef CARTOGRAPHIC_SUPPORT
/*
 * Fetch the private Cartographic IFD from the file.
 */
static void
CARTReadDirectory(tif)
	TIFF *tif;
{
	TIFFDirectory *td = &tif->tif_dir;
	register TIFFDirEntry *dp;
	register int n;
	TIFFDirEntry *dir;
	TIFFFieldInfo *fip;
	u_short dircount;
	long off;
	int ok;

	dir=TIFFGetDirectoryEntries(tif,td->td_carto_ifd_offset,&dircount,0L);
	if (!dir) {
		TIFFError(tif->tif_name,
			"Error accessing CART directory entries.");
		return;
	}

	for (fip = (TIFFFieldInfo *)cartFieldInfo, dp = dir, n = dircount; n > 0; n--, dp++) {
		/*
		 * Find the field information entry for this tag.
		 */
		off = TIFFDirFieldOffset(tif,dp,(TIFFFieldInfo *)cartFieldInfo,fip);
		if (off >= 0)
			fip=(TIFFFieldInfo *)cartFieldInfo+off;
		else switch (off)
		{
			case STATUS_UNKNOWN:
				fip = (TIFFFieldInfo *)cartFieldInfo; /* restart search */
				/* fall through to.. */
			case STATUS_IGNORE:
				dp->tdir_tag = IGNORE;
				continue;
		}
		ok= TIFFFetchNormalTag(tif, dp);
	}
	if (ok) tif->tif_flags |= TIFF_HASCARTTAGS;

	if (dir)
		free((char *)dir);
}


/*
* For Backward compatibility -- Look at image description
* tag to see if Cartographic data was stored there and
* if so, convert it to the new format.
*/
static void
CARTReadOldDirectory(tif)
	TIFF *tif;
{
	char *cart_string;
	int nItems;
	u_short	projectiontype;
	tiff_u_long	proj_xpos;
	tiff_u_long	proj_ypos;
	tiff_u_long	latitude;
	tiff_u_long	longitude;
	tiff_u_long  xPix[2],yPix[2];
	double	xpixperangle;
	double	ypixperangle;
	char *sptr;
	
	TIFFGetField(tif, TIFFTAG_IMAGEDESCRIPTION, &cart_string);
#ifdef vms
	/* VAX C doesn't understand scanf "%u" !!! */

	if (strncmp( cart_string, "CART_RESOURCE",13)==0)
	{
		sptr = cart_string+14;
		projectiontype = GetLong(&sptr);
		longitude = GetLong(&sptr);
		latitude = GetLong(&sptr);
		xPix[0] = GetLong(&sptr);
		xPix[1] = GetLong(&sptr);
		yPix[0] = GetLong(&sptr);
		yPix[1] = GetLong(&sptr);
		proj_xpos = GetLong(&sptr);
		proj_ypos = GetLong(&sptr);
		nItems = 9;
        }
	else nItems=0;
#else
	nItems = sscanf(cart_string,
		"CART_RESOURCE(%hu,%ld,%ld,%ld,%lu,%ld,%lu,%ld,%ld)",
		 &projectiontype,&longitude,&latitude,
		 xPix,xPix+1,  yPix,yPix+1, &proj_xpos,&proj_ypos);
#endif
	if (nItems == 9)
	{
		xpixperangle = (double)xPix[0] + (double)(xPix[1])/(tiff_u_long)(0xFFFFFFFF);
		ypixperangle = (double)yPix[0] + (double)(yPix[1])/(tiff_u_long)(0xFFFFFFFF);
		xpixperangle /= 36e5;  /* Degrees to pix-per-Thous-Secnd */
		ypixperangle /= 36e5;
		TIFFSetField(tif, CARTTAG_PROJECTIONTYPE, projectiontype);
		TIFFSetField(tif, CARTTAG_PROJ_XPOS, proj_xpos);
		TIFFSetField(tif, CARTTAG_PROJ_YPOS, proj_ypos);
		TIFFSetField(tif, CARTTAG_LATITUDE, latitude);
		TIFFSetField(tif, CARTTAG_LONGITUDE, longitude);
		TIFFSetField(tif, CARTTAG_XPIXPERANGLE, (float)xpixperangle);
		TIFFSetField(tif, CARTTAG_YPIXPERANGLE, (float)ypixperangle);
		tif->tif_flags |= TIFF_HASCARTTAGS;
	}
}
#endif /* CARTO */

/*
 * Fetch an ASCII item from the file.
 */
static
TIFFFetchString(tif, dir, cp)
	TIFF *tif;
	TIFFDirEntry *dir;
	char *cp;
{
	if (dir->tdir_count <= 4) {
		tiff_u_long l = dir->tdir_offset;
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabLong(&l);
		mybcopy(&l, cp, dir->tdir_count);
		return (1);
	}
	return (TIFFFetchData(tif, dir, cp));
}

/*
 * Convert numerator+denominator to float.
 */
static int
cvtRational(tif, dir, num, denom, rv)
	TIFF *tif;
	TIFFDirEntry *dir;
	tiff_u_long num, denom;
	float *rv;
{
	if (denom == 0) {
		TIFFError(tif->tif_name,
		    "%s: Rational with zero denominator (num = %lu)",
		    TIFFFieldWithTag(dir->tdir_tag)->field_name, num);
		return (0);
	} else {
		if (dir->tdir_type == TIFF_RATIONAL)
			*rv = ((float)num / (float)denom);
		else
			*rv = ((float)(long)num / (float)(long)denom);
		return (1);
	}
}

/*
 * Fetch a rational item from the file
 * at offset off and return the value
 * as a floating point number.
 */
static float
TIFFFetchRational(tif, dir)
	TIFF *tif;
	TIFFDirEntry *dir;
{
	tiff_u_long l[2];
	float v;

	return (!TIFFFetchData(tif, dir, (char *)l) ||
	    !cvtRational(tif, dir, l[0], l[1], &v) ? 1. : v);
}

/*
 * Fetch a single floating point value
 * from the offset field and return it
 * as a native float.
 */
static float
TIFFFetchFloat(tif, dir)
	TIFF *tif;
	TIFFDirEntry *dir;
{
	float v = (float)
	    TIFFExtractData(tif, dir->tdir_type, dir->tdir_offset);
	TIFFCvtIEEEFloatToNative(tif, 1, &v);
	return (v);
}

/*
 * Fetch an array of BYTE or SBYTE values.
 */
static
TIFFFetchByteArray(tif, dir, v)
	TIFF *tif;
	TIFFDirEntry *dir;
	u_short v[];
{

	if (dir->tdir_count <= 4) {
		/*
		 * Extract data from offset field.
		 */
		if (tif->tif_header.tiff_magic == TIFF_BIGENDIAN) {
			switch (dir->tdir_count) {
			case 4: v[3] = dir->tdir_offset & 0xff;
			case 3: v[2] = (dir->tdir_offset >> 8) & 0xff;
			case 2: v[1] = (dir->tdir_offset >> 16) & 0xff;
			case 1: v[0] = dir->tdir_offset >> 24;
			}
		} else {
			switch (dir->tdir_count) {
			case 4: v[3] = dir->tdir_offset >> 24;
			case 3: v[2] = (dir->tdir_offset >> 16) & 0xff;
			case 2: v[1] = (dir->tdir_offset >> 8) & 0xff;
			case 1: v[0] = dir->tdir_offset & 0xff;
			}
		}
		return (1);
	} else
		return (TIFFFetchData(tif, dir, (char *)v));	/* XXX */
}

/*
 * Fetch an array of SHORT or SSHORT values.
 */
static
TIFFFetchShortArray(tif, dir, v)
	TIFF *tif;
	TIFFDirEntry *dir;
	u_short v[];
{
	int ok;
	
	if (dir->tdir_count <= 2) {
		if (tif->tif_header.tiff_magic == TIFF_BIGENDIAN) {
			switch (dir->tdir_count) {
			case 2: v[1] = dir->tdir_offset & 0xffff;
			case 1: v[0] = dir->tdir_offset >> 16;
			}
		} else {
			switch (dir->tdir_count) {
			case 2: v[1] = dir->tdir_offset >> 16;
			case 1: v[0] = dir->tdir_offset & 0xffff;
			}
		}
		return (1);
	} else
	{
		ok = TIFFFetchData(tif, dir, (char *)v);
		TIFFUnPackShort((short *)v,dir->tdir_count);
		return (ok);
	}
}

/*
 * Fetch an array of LONG or SLONG values.
 */
static
TIFFFetchLongArray(tif, dir, v)
	TIFF *tif;
	TIFFDirEntry *dir;
	tiff_u_long v[];
{
	if (dir->tdir_count == 1) {
		v[0] = dir->tdir_offset;
		return (1);
	} else
		return (TIFFFetchData(tif, dir, (char *)v));
}

/*
 * Fetch an array of RATIONAL or SRATIONAL values.
 */
static
TIFFFetchRationalArray(tif, dir, v)
	TIFF *tif;
	TIFFDirEntry *dir;
	float v[];
{
	int ok = 0;
	tiff_u_long *l;

	l = (tiff_u_long *)CheckMalloc(tif,
	    dir->tdir_count*tiffDataWidth[dir->tdir_type],
	    "to fetch array of rationals");
	if (l) {
		if (TIFFFetchData(tif, dir, (char *)l)) {
			tiff_u_long i;
			for (i = 0; i < dir->tdir_count; i++) {
				ok = cvtRational(tif, dir,
				    l[2*i+0], l[2*i+1], &v[i]);
				if (!ok)
					break;
			}
		}
		free((char *)l);
	}
	return (ok);
}

/*
 * Fetch an array of FLOAT values.
 */
static
TIFFFetchFloatArray(tif, dir, v)
	TIFF *tif;
	TIFFDirEntry *dir;
	float v[];
{
	if (TIFFFetchData(tif, dir, (char *)v)) {
		TIFFCvtIEEEFloatToNative(tif, dir->tdir_count, v);
		return (1);
	} else
		return (0);
}

/*
 * Fetch a tag that is not handled by special case code.
 *
 * NB: DOUBLE and UNDEFINED types are not handled.
 */
static
TIFFFetchNormalTag(tif, dp)
	TIFF *tif;
	TIFFDirEntry *dp;
{
	static char mesg[] = "to fetch tag value";
	int ok = 0;

	if (dp->tdir_count > 1) {		/* array of values */
		char *cp = NULL;

		switch (dp->tdir_type) {
		case TIFF_BYTE:
		case TIFF_SBYTE:
			/* NB: always expand BYTE values to shorts */
			cp = CheckMalloc(tif,
			    dp->tdir_count * sizeof (u_short), mesg);
			ok = cp && TIFFFetchByteArray(tif, dp, (u_short *)cp);
			break;
		case TIFF_SHORT:
		case TIFF_SSHORT:
			cp = CheckMalloc(tif,
			    dp->tdir_count * sizeof (u_short), mesg);
			ok = cp && TIFFFetchShortArray(tif, dp, (u_short *)cp);
			break;
		case TIFF_LONG:
		case TIFF_SLONG:
			cp = CheckMalloc(tif,
			    dp->tdir_count * sizeof (tiff_u_long), mesg);
			ok = cp && TIFFFetchLongArray(tif, dp, (tiff_u_long *)cp);
			break;
		case TIFF_RATIONAL:
		case TIFF_SRATIONAL:
			cp = CheckMalloc(tif,
			    dp->tdir_count * sizeof (float), mesg);
			ok = cp && TIFFFetchRationalArray(tif, dp, (float *)cp);
			break;
		case TIFF_FLOAT:
			cp = CheckMalloc(tif,
			    dp->tdir_count * sizeof (float), mesg);
			ok = cp && TIFFFetchFloatArray(tif, dp, (float *)cp);
			break;
		case TIFF_ASCII:
			/*
			 * Some vendors write strings w/o the trailing
			 * NULL byte, so always append one just in case.
			 */
			cp = CheckMalloc(tif, dp->tdir_count+1, mesg);
			if (ok = (cp && TIFFFetchString(tif, dp, cp)))
				cp[dp->tdir_count] = '\0';	/* XXX */
			break;
		}
		if (ok)
			ok = TIFFSetField(tif, dp->tdir_tag, cp);
		if (cp != NULL)
			free(cp);
	} else if (CheckDirCount(tif, dp, 1)) {	/* singleton value */
		char c[2];
		switch (dp->tdir_type) {
		case TIFF_BYTE:
		case TIFF_SBYTE:
		case TIFF_SHORT:
		case TIFF_SSHORT:
			ok = TIFFSetField(tif, dp->tdir_tag, (int)
		  TIFFExtractData(tif, dp->tdir_type, dp->tdir_offset));
			break;
		case TIFF_LONG:
		case TIFF_SLONG:
			ok = TIFFSetField(tif, dp->tdir_tag, (tiff_u_long)
		  TIFFExtractData(tif, dp->tdir_type, dp->tdir_offset));
			break;
		case TIFF_RATIONAL:
		case TIFF_SRATIONAL:
			ok = TIFFSetField(tif, dp->tdir_tag,
			    TIFFFetchRational(tif, dp));
			break;
		case TIFF_FLOAT:
			ok = TIFFSetField(tif, dp->tdir_tag,
			    TIFFFetchFloat(tif, dp));
			break;
		case TIFF_ASCII:
			if (ok = (TIFFFetchString(tif, dp, c))) {
				c[1] = '\0';		/* XXX paranoid */
				ok = TIFFSetField(tif, dp->tdir_tag, c);
			}
			break;
		}
	}
	return (ok);
}

/*
 * Fetch a tag that could be long or short.
 */
static
TIFFFetchLongShortTag(tif, dp)
	TIFF *tif;
	TIFFDirEntry *dp;
{
	static char mesg[] = "to fetch tag value";
	int ok = 0;

	ok = TIFFSetField(tif, dp->tdir_tag, (tiff_u_long)
		  TIFFExtractData(tif, dp->tdir_type, dp->tdir_offset));

	return (ok);
}




/*
 * Fetch samples/pixel short values for 
 * the specified tag and verify that
 * all values are the same.
 */
static
TIFFFetchPerSampleShorts(tif, dir, pl)
	TIFF *tif;
	TIFFDirEntry *dir;
	long *pl;
{
	u_short v[4];
	int samples = tif->tif_dir.td_samplesperpixel;

	if (CheckDirCount(tif, dir, (tiff_u_long)samples) &&
	    TIFFFetchShortArray(tif, dir, v)) {
		int i;
		for (i = 1; i < samples; i++)
			if (v[i] != v[0]) {
				TIFFError(tif->tif_name,
		"Cannot handle different per-sample values for field \"%s\"",
				   TIFFFieldWithTag(dir->tdir_tag)->field_name);
				return (0);
			}
		*pl = v[0];
		return (1);
	}
	return (0);
}

/*
 * Fetch a set of offsets or lengths.
 * While this routine says "strips",
 * in fact it's also used for tiles.
 */
static
TIFFFetchStripThing(tif, dir, nstrips, lpp)
	TIFF *tif;
	TIFFDirEntry *dir;
	long nstrips;
	tiff_u_long **lpp;
{
	register tiff_u_long *lp;
	int status;

	if (!CheckDirCount(tif, dir, nstrips))
		return (0);
	/*
	 * Allocate space for strip information.
	 */
	if (*lpp == NULL &&
	    (*lpp = (tiff_u_long *)CheckMalloc(tif,
	      nstrips * sizeof (tiff_u_long), "for strip array")) == NULL)
		return (0);
	lp = *lpp;
	if (dir->tdir_type == (int)TIFF_SHORT) {
		/*
		 * Handle short->long expansion.
		 */
		u_short *dp = (u_short *)CheckMalloc(tif,
		    dir->tdir_count* sizeof (u_short), "to fetch strip tag");
		if (dp == NULL)
			return (0);
		if (status = TIFFFetchShortArray(tif, dir, dp)) {
			register u_short *wp = dp;
			while (nstrips-- > 0)
				*lp++ = *wp++;
		}
		free((char *)dp);
	} else
		status = TIFFFetchLongArray(tif, dir, lp);
	return (status);
}

#ifdef COLORIMETRY_SUPPORT
static
TIFFFetchRefBlackWhite(tif, dir)
	TIFF *tif;
	TIFFDirEntry *dir;
{
	static char mesg[] = "for \"ReferenceBlackWhite\" array";
	char *cp;
	int ok;

	if (!CheckDirCount(tif, dir, 2*tif->tif_dir.td_samplesperpixel))
		return (0);
	if (dir->tdir_type == TIFF_RATIONAL)
		return (TIFFFetchNormalTag(tif, dir));
	/*
	 * Handle LONG's for backward compatibility.
	 */
	cp = CheckMalloc(tif, dir->tdir_count * sizeof (tiff_u_long), mesg);
	if (ok = (cp && TIFFFetchLongArray(tif, dir, (tiff_u_long *)cp))) {
		float *fp = (float *)
		    CheckMalloc(tif, dir->tdir_count * sizeof (float), mesg);
		if (ok = (fp != NULL)) {
			int i;
			for (i = 0; i < dir->tdir_count; i++)
				fp[i] = (float)((tiff_u_long *)cp)[i];
			ok = TIFFSetField(tif, dir->tdir_tag, fp);
			free((char *)fp);
		}
	}
	if (cp)
		free(cp);
	return (ok);
}
#endif

#ifdef JPEG_SUPPORT
/*
 * Fetch the JPEG Quantization tables
 * for the specified directory entry.
 * Storage for the td_qtab array is
 * allocated as a side effect.
 */
static
TIFFFetchJPEGQTables(tif, dir)
	TIFF *tif;
	TIFFDirEntry *dir;
{
	TIFFDirectory *td = &tif->tif_dir;
	long off[4];
	int i, j;
	TIFFDirEntry tdir;
	char *qmat;

	if (dir->tdir_count > 1) {
		/* XXX verify count <= 4 */
		if (!TIFFFetchData(tif, dir, (char *)off))
			return (0);
	} else
		off[0] = dir->tdir_offset;
	/*
	 * We don't share per-component q matrices because
	 * (besides complicating this logic even more), it
	 * would make it very painful if the user does a ``set''.
	 */
	td->td_qtab = (u_char **)CheckMalloc(tif,
	    dir->tdir_count*(sizeof (u_char *) + 64*sizeof (u_char)),
	    "for JPEG Q table");
	if (td->td_qtab == NULL)
		return (0);
	tdir.tdir_type = TIFF_BYTE;
	tdir.tdir_count = 64;
	qmat = (((char *)td->td_qtab) + dir->tdir_count*sizeof (u_char *));
	for (i = 0; i < dir->tdir_count; i++) {
		td->td_qtab[i] = (u_char *)qmat;
		tdir.tdir_offset = off[i];
		if (!TIFFFetchData(tif, &tdir, qmat))
			return (0);
		qmat += 64*sizeof (u_char);
	}
	return (1);
}

/*
 * Fetch JPEG Huffman code tables for the
 * specified directory entry.  Storage for
 * the tables are allocated as a side effect.
 */
static
TIFFFetchJPEGCTables(tif, dir, ptab)
	TIFF *tif;
	TIFFDirEntry *dir;
	u_char ***ptab;
{
	long off[4];
	int i, j, ncodes;
	TIFFDirEntry tdir;
	char *tab;

	if (dir->tdir_count > 1) {
		/* XXX verify count <= 4 */
		if (!TIFFFetchData(tif, dir, (char *)off))
			return (0);
	} else
		off[0] = dir->tdir_offset;
	/*
	 * We don't share per-component tables because
	 * (besides complicating this logic even more), it
	 * would make it very painful if the user does a
	 * ``set''.  Note also that we don't try to optimize
	 * storage of the tables -- we just allocate enough
	 * space to hold the largest possible.  All this
	 * stuff is so complicated 'cuz the tag is defined
	 * to be compatible with the JPEG table format,
	 * rather than something that fits well into the
	 * structure of TIFF -- argh!
	 */
	*ptab = (u_char **)CheckMalloc(tif, dir->tdir_count*
	    (sizeof (u_char *) + (16+256)*sizeof (u_char)),
	    "for JPEG Huffman table");
	if (*ptab == NULL)
		return (0);
	tdir.tdir_type = TIFF_BYTE;
	tab = (((char *)*ptab) + dir->tdir_count*sizeof (u_char *));
	for (i = 0; i < dir->tdir_count; i++) {
		(*ptab)[i] = (u_char *)tab;
		tdir.tdir_offset = off[i];
		tdir.tdir_count = 16;
		/*
		 * We must fetch the array that holds the
		 * count of codes for each bit length first
		 * and the count up the number of codes that
		 * are in the variable length table.  This
		 * information is implicit in the JPEG format
		 * 'cuz it's preceded by a length field.
		 */
		if (!TIFFFetchData(tif, &tdir, tab))	/* count array */
			return (0);
		for (ncodes = 0, j = 0; j < 16; j++)
			ncodes += tab[j];
		/*
		 * Adjust offsets and fetch codes separately.
		 */
		tdir.tdir_offset += 16;
		tdir.tdir_count = ncodes;
		tab += 16;
		if (!TIFFFetchData(tif, &tdir, tab))
			return (0);
		tab += ncodes;
	}
	return (1);
}
#endif

/*
 * Accept matteing-only ExtraSamples tag.
 */
static
TIFFFetchExtraSamples(tif, dp)
	TIFF *tif;
	TIFFDirEntry *dp;
{
	int type;
	
	if (dp->tdir_count != 1) {
		TIFFError(tif->tif_name,
		    "Can not handle more than 1 extra sample/pixel");
		return (0);
	}
	type = TIFFExtractData(tif, dp->tdir_type, dp->tdir_offset);
	if (type != EXTRASAMPLE_ASSOCALPHA) {
		TIFFError(tif->tif_name,
		    "Can only handle associated-alpha extra samples");
		return (0);
	}
	return (TIFFSetField(tif, TIFFTAG_MATTEING, 1));
}

/* end dir_read */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_diri.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_dirinfo.c,v 1.17 92/03/20 11:25:08 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * Known Directory Tag Support.
 */
#include "tiffiop.h"
#include "prototypes.h"

#ifndef TRUE
#define	TRUE	1
#define	FALSE	0
#endif


/*
 * NB: THIS ARRAY IS ASSUMED TO BE SORTED BY TAG.
 *     Also, if a tag can have both LONG and SHORT types
 *     then the LONG must be placed before the SHORT for
 *     writing to work properly.
 */
const TIFFFieldInfo tiffFieldInfo[] = {
    { TIFFTAG_SUBFILETYPE,	 1, 1, TIFF_LONG,	FIELD_SUBFILETYPE,
      TRUE,	"SubfileType" },
/* XXX SHORT for compatibility w/ old versions of the library */
    { TIFFTAG_SUBFILETYPE,	 1, 1, TIFF_SHORT,	FIELD_SUBFILETYPE,
      TRUE,	"SubfileType" },
    { TIFFTAG_OSUBFILETYPE,	 1, 1, TIFF_SHORT,	FIELD_SUBFILETYPE,
      TRUE,	"OldSubfileType" },
    { TIFFTAG_IMAGEWIDTH,	 1, 1, TIFF_LONG,	FIELD_IMAGEDIMENSIONS,
      FALSE,	"ImageWidth" },
    { TIFFTAG_IMAGEWIDTH,	 1, 1, TIFF_SHORT,	FIELD_IMAGEDIMENSIONS,
      FALSE,	"ImageWidth" },
    { TIFFTAG_IMAGELENGTH,	 1, 1, TIFF_LONG,	FIELD_IMAGEDIMENSIONS,
      TRUE,	"ImageLength" },
    { TIFFTAG_IMAGELENGTH,	 1, 1, TIFF_SHORT,	FIELD_IMAGEDIMENSIONS,
      TRUE,	"ImageLength" },
    { TIFFTAG_BITSPERSAMPLE,	-1,-1, TIFF_SHORT,	FIELD_BITSPERSAMPLE,
      FALSE,	"BitsPerSample" },
    { TIFFTAG_COMPRESSION,	-1, 1, TIFF_SHORT,	FIELD_COMPRESSION,
      FALSE,	"Compression" },
    { TIFFTAG_PHOTOMETRIC,	 1, 1, TIFF_SHORT,	FIELD_PHOTOMETRIC,
      FALSE,	"PhotometricInterpretation" },
    { TIFFTAG_THRESHHOLDING,	 1, 1, TIFF_SHORT,	FIELD_THRESHHOLDING,
      TRUE,	"Threshholding" },
    { TIFFTAG_CELLWIDTH,	 1, 1, TIFF_SHORT,	FIELD_IGNORE,
      TRUE,	"CellWidth" },
    { TIFFTAG_CELLLENGTH,	 1, 1, TIFF_SHORT,	FIELD_IGNORE,
      TRUE,	"CellLength" },
    { TIFFTAG_FILLORDER,	 1, 1, TIFF_SHORT,	FIELD_FILLORDER,
      FALSE,	"FillOrder" },
    { TIFFTAG_DOCUMENTNAME,	-1,-1, TIFF_ASCII,	FIELD_DOCUMENTNAME,
      TRUE,	"DocumentName" },
    { TIFFTAG_IMAGEDESCRIPTION,	-1,-1, TIFF_ASCII,	FIELD_IMAGEDESCRIPTION,
      TRUE,	"ImageDescription" },
    { TIFFTAG_MAKE,		-1,-1, TIFF_ASCII,	FIELD_MAKE,
      TRUE,	"Make" },
    { TIFFTAG_MODEL,		-1,-1, TIFF_ASCII,	FIELD_MODEL,
      TRUE,	"Model" },
    { TIFFTAG_STRIPOFFSETS,	-1,-1, TIFF_LONG,	FIELD_STRIPOFFSETS,
      FALSE,	"StripOffsets" },
    { TIFFTAG_STRIPOFFSETS,	-1,-1, TIFF_SHORT,	FIELD_STRIPOFFSETS,
      FALSE,	"StripOffsets" },
    { TIFFTAG_ORIENTATION,	 1, 1, TIFF_SHORT,	FIELD_ORIENTATION,
      FALSE,	"Orientation" },
    { TIFFTAG_SAMPLESPERPIXEL,	 1, 1, TIFF_SHORT,	FIELD_SAMPLESPERPIXEL,
      FALSE,	"SamplesPerPixel" },
    { TIFFTAG_ROWSPERSTRIP,	 1, 1, TIFF_LONG,	FIELD_ROWSPERSTRIP,
      FALSE,	"RowsPerStrip" },
    { TIFFTAG_ROWSPERSTRIP,	 1, 1, TIFF_SHORT,	FIELD_ROWSPERSTRIP,
      FALSE,	"RowsPerStrip" },
    { TIFFTAG_STRIPBYTECOUNTS,	-1,-1, TIFF_LONG,	FIELD_STRIPBYTECOUNTS,
      FALSE,	"StripByteCounts" },
    { TIFFTAG_STRIPBYTECOUNTS,	-1,-1, TIFF_SHORT,	FIELD_STRIPBYTECOUNTS,
      FALSE,	"StripByteCounts" },
    { TIFFTAG_MINSAMPLEVALUE,	-2,-1, TIFF_SHORT,	FIELD_MINSAMPLEVALUE,
      TRUE,	"MinSampleValue" },
    { TIFFTAG_MAXSAMPLEVALUE,	-2,-1, TIFF_SHORT,	FIELD_MAXSAMPLEVALUE,
      TRUE,	"MaxSampleValue" },
    { TIFFTAG_XRESOLUTION,	 1, 1, TIFF_RATIONAL,	FIELD_RESOLUTION,
      FALSE,	"XResolution" },
    { TIFFTAG_YRESOLUTION,	 1, 1, TIFF_RATIONAL,	FIELD_RESOLUTION,
      FALSE,	"YResolution" },
    { TIFFTAG_PLANARCONFIG,	 1, 1, TIFF_SHORT,	FIELD_PLANARCONFIG,
      FALSE,	"PlanarConfiguration" },
    { TIFFTAG_PAGENAME,		-1,-1, TIFF_ASCII,	FIELD_PAGENAME,
      TRUE,	"PageName" },
    { TIFFTAG_XPOSITION,	 1, 1, TIFF_RATIONAL,	FIELD_POSITION,
      TRUE,	"XPosition" },
    { TIFFTAG_YPOSITION,	 1, 1, TIFF_RATIONAL,	FIELD_POSITION,
      TRUE,	"YPosition" },
    { TIFFTAG_FREEOFFSETS,	-1,-1, TIFF_LONG,	FIELD_IGNORE,
      FALSE,	"FreeOffsets" },
    { TIFFTAG_FREEBYTECOUNTS,	-1,-1, TIFF_LONG,	FIELD_IGNORE,
      FALSE,	"FreeByteCounts" },
    { TIFFTAG_GRAYRESPONSEUNIT,	 1, 1, TIFF_SHORT,	FIELD_IGNORE,
      TRUE,	"GrayResponseUnit" },
    { TIFFTAG_GRAYRESPONSECURVE,-1,-1, TIFF_SHORT,	FIELD_IGNORE,
      TRUE,	"GrayResponseCurve" },
    { TIFFTAG_GROUP3OPTIONS,	 1, 1, TIFF_LONG,	FIELD_GROUP3OPTIONS,
      FALSE,	"Group3Options" },
    { TIFFTAG_GROUP4OPTIONS,	 1, 1, TIFF_LONG,	FIELD_GROUP4OPTIONS,
      FALSE,	"Group4Options" },
    { TIFFTAG_RESOLUTIONUNIT,	 1, 1, TIFF_SHORT,	FIELD_RESOLUTIONUNIT,
      FALSE,	"ResolutionUnit" },
    { TIFFTAG_PAGENUMBER,	 2, 2, TIFF_SHORT,	FIELD_PAGENUMBER,
      TRUE,	"PageNumber" },
    { TIFFTAG_COLORRESPONSEUNIT, 1, 1, TIFF_SHORT,	FIELD_IGNORE,
      TRUE,	"ColorResponseUnit" },
#ifdef COLORIMETRY_SUPPORT
    { TIFFTAG_TRANSFERFUNCTION,	-1,-1, TIFF_SHORT,	FIELD_TRANSFERFUNCTION,
      TRUE,	"TransferFunction" },
#endif
    { TIFFTAG_SOFTWARE,		-1,-1, TIFF_ASCII,	FIELD_SOFTWARE,
      TRUE,	"Software" },
    { TIFFTAG_DATETIME,		20,20, TIFF_ASCII,	FIELD_DATETIME,
      TRUE,	"DateTime" },
    { TIFFTAG_ARTIST,		-1,-1, TIFF_ASCII,	FIELD_ARTIST,
      TRUE,	"Artist" },
    { TIFFTAG_HOSTCOMPUTER,	-1,-1, TIFF_ASCII,	FIELD_HOSTCOMPUTER,
      TRUE,	"HostComputer" },
    { TIFFTAG_PREDICTOR,	 1, 1, TIFF_SHORT,	FIELD_PREDICTOR,
      FALSE,	"Predictor" },
#ifdef COLORIMETRY_SUPPORT
    { TIFFTAG_WHITEPOINT,	 2, 2, TIFF_RATIONAL,FIELD_WHITEPOINT,
      TRUE,	"WhitePoint" },
    { TIFFTAG_PRIMARYCHROMATICITIES,6,6,TIFF_RATIONAL,FIELD_PRIMARYCHROMAS,
      TRUE,	"PrimaryChromaticities" },
#endif
    { TIFFTAG_COLORMAP,		-1,-1, TIFF_SHORT,	FIELD_COLORMAP,
      TRUE,	"ColorMap" },
    { TIFFTAG_HALFTONEHINTS,	 2, 2, TIFF_SHORT,	FIELD_HALFTONEHINTS,
      TRUE,	"HalftoneHints" },
    { TIFFTAG_TILEWIDTH,	 1, 1, TIFF_LONG,	FIELD_TILEDIMENSIONS,
      FALSE,	"TileWidth" },
    { TIFFTAG_TILEWIDTH,	 1, 1, TIFF_SHORT,	FIELD_TILEDIMENSIONS,
      FALSE,	"TileWidth" },
    { TIFFTAG_TILELENGTH,	 1, 1, TIFF_LONG,	FIELD_TILEDIMENSIONS,
      FALSE,	"TileLength" },
    { TIFFTAG_TILELENGTH,	 1, 1, TIFF_SHORT,	FIELD_TILEDIMENSIONS,
      FALSE,	"TileLength" },
    { TIFFTAG_TILEOFFSETS,	-1, 1, TIFF_LONG,	FIELD_STRIPOFFSETS,
      FALSE,	"TileOffsets" },
    { TIFFTAG_TILEBYTECOUNTS,	-1, 1, TIFF_LONG,	FIELD_STRIPBYTECOUNTS,
      FALSE,	"TileByteCounts" },
    { TIFFTAG_TILEBYTECOUNTS,	-1, 1, TIFF_SHORT,	FIELD_STRIPBYTECOUNTS,
      FALSE,	"TileByteCounts" },
    { TIFFTAG_BADFAXLINES,	 1, 1, TIFF_LONG,	FIELD_BADFAXLINES,
      TRUE,	"BadFaxLines" },
    { TIFFTAG_BADFAXLINES,	 1, 1, TIFF_SHORT,	FIELD_BADFAXLINES,
      TRUE,	"BadFaxLines" },
    { TIFFTAG_CLEANFAXDATA,	 1, 1, TIFF_SHORT,	FIELD_CLEANFAXDATA,
      TRUE,	"CleanFaxData" },
    { TIFFTAG_CONSECUTIVEBADFAXLINES,1,1, TIFF_LONG,FIELD_BADFAXRUN,
      TRUE,	"ConsecutiveBadFaxLines" },
    { TIFFTAG_CONSECUTIVEBADFAXLINES,1,1, TIFF_SHORT,FIELD_BADFAXRUN,
      TRUE,	"ConsecutiveBadFaxLines" },
#ifdef CMYK_SUPPORT		/* 6.0 CMYK tags */
    { TIFFTAG_INKSET,		 1, 1, TIFF_SHORT,	FIELD_INKSET,
      FALSE,	"InkSet" },
    { TIFFTAG_INKNAMES,		-1,-1, TIFF_ASCII,	FIELD_INKNAMES,
      TRUE,	"InkNames" },
    { TIFFTAG_DOTRANGE,		 2, 2, TIFF_SHORT,	FIELD_DOTRANGE,
      FALSE,	"DotRange" },
    { TIFFTAG_DOTRANGE,		 2, 2, TIFF_BYTE,	FIELD_DOTRANGE,
      FALSE,	"DotRange" },
    { TIFFTAG_TARGETPRINTER,	-1,-1, TIFF_ASCII,	FIELD_TARGETPRINTER,
      TRUE,	"TargetPrinter" },
#endif
    { TIFFTAG_EXTRASAMPLES,	-1,-1, TIFF_SHORT,	FIELD_MATTEING,
      FALSE,	"ExtraSamples" },
    { TIFFTAG_SAMPLEFORMAT,	-1,-1, TIFF_SHORT,	FIELD_SAMPLEFORMAT,
      FALSE,	"SampleFormat" },
#ifdef notdef
    { TIFFTAG_SMINSAMPLEVALUE,	-2,-1, TIFF_ANY,	FIELD_SMINSAMPLEVALUE,
      TRUE,	"SMinSampleValue" },
    { TIFFTAG_SMAXSAMPLEVALUE,	-2,-1, TIFF_ANY,	FIELD_SMAXSAMPLEVALUE,
      TRUE,	"SMaxSampleValue" },
#endif
#ifdef JPEG_SUPPORT		/* 6.0 JPEG tags */
    { TIFFTAG_JPEGPROC,		 1, 1, TIFF_SHORT,	FIELD_JPEGPROC,
      FALSE,	"JPEGProc" },
    { TIFFTAG_JPEGIFOFFSET,	 1, 1, TIFF_LONG,	FIELD_IGNORE,
      FALSE,	"JPEGInterchangeFormat" },
    { TIFFTAG_JPEGIFBYTECOUNT,	 1, 1, TIFF_LONG,	FIELD_IGNORE,
      FALSE,	"JPEGInterchangeFormatLength" },
    { TIFFTAG_JPEGRESTARTINTERVAL,1,1, TIFF_SHORT,FIELD_JPEGRESTARTINTERVAL,
      FALSE,	"JPEGRestartInterval" },
    { TIFFTAG_JPEGQTABLES,	-2,-1, TIFF_LONG,	FIELD_JPEGQTABLES,
      FALSE,	"JPEGQTables" },
    { TIFFTAG_JPEGDCTABLES,	-2,-1, TIFF_LONG,	FIELD_JPEGDCTABLES,
      FALSE,	"JPEGDCTables" },
    { TIFFTAG_JPEGACTABLES,	-2,-1, TIFF_LONG,	FIELD_JPEGACTABLES,
      FALSE,	"JPEGACTables" },
#endif
#ifdef YCBCR_SUPPORT		/* 6.0 YCbCr tags */
    { TIFFTAG_YCBCRCOEFFICIENTS, 3, 3, TIFF_RATIONAL,	FIELD_YCBCRCOEFFICIENTS,
      FALSE,	"YCbCrCoefficients" },
    { TIFFTAG_YCBCRSUBSAMPLING,	 2, 2, TIFF_SHORT,	FIELD_YCBCRSUBSAMPLING,
      FALSE,	"YCbCrSubsampling" },
    { TIFFTAG_YCBCRPOSITIONING,	 1, 1, TIFF_SHORT,	FIELD_YCBCRPOSITIONING,
      FALSE,	"YCbCrPositioning" },
#endif
#ifdef COLORIMETRY_SUPPORT
    { TIFFTAG_REFERENCEBLACKWHITE,-1,-1,TIFF_RATIONAL,	FIELD_REFBLACKWHITE,
      FALSE,	"ReferenceBlackWhite" },
/* XXX temporarily accept LONG for backwards compatibility */
    { TIFFTAG_REFERENCEBLACKWHITE,-1,-1,TIFF_LONG,	FIELD_REFBLACKWHITE,
      FALSE,	"ReferenceBlackWhite" },
#endif
/* begin SGI tags */
    { TIFFTAG_MATTEING,		 1, 1, TIFF_SHORT,	FIELD_MATTEING,
      FALSE,	"Matteing" },
    { TIFFTAG_DATATYPE,		-2,-1, TIFF_SHORT,	FIELD_SAMPLEFORMAT,
      FALSE,	"DataType" },
    { TIFFTAG_IMAGEDEPTH,	 1, 1, TIFF_LONG,	FIELD_IMAGEDEPTH,
      FALSE,	"ImageDepth" },
    { TIFFTAG_IMAGEDEPTH,	 1, 1, TIFF_SHORT,	FIELD_IMAGEDEPTH,
      FALSE,	"ImageDepth" },
    { TIFFTAG_TILEDEPTH,	 1, 1, TIFF_LONG,	FIELD_TILEDEPTH,
      FALSE,	"TileDepth" },
    { TIFFTAG_TILEDEPTH,	 1, 1, TIFF_SHORT,	FIELD_TILEDEPTH,
      FALSE,	"TileDepth" },
/* end SGI tags */
#ifdef CARTOGRAPHIC_SUPPORT
    { TIFFTAG_CARTO_IFD_OFFSET, 1, 1, TIFF_LONG,	FIELD_CARTO_IFD_OFFSET,
      TRUE,	"OffsetToCartographicIFD" },
#endif
    { 0 }
};

#ifdef CARTOGRAPHIC_SUPPORT
/*
 * TIFF tags valid only in the private carto IFD.
 * This is a separate structure so that TIFFWriteDirectory
 * will not try to write these out into the public
 * TIFF directory.
 */
const TIFFFieldInfo cartFieldInfo[] = {
    { CARTTAG_PROJECTIONTYPE,	 1, 1, TIFF_SHORT,	    FIELD_PROJECTIONTYPE,
	  TRUE,	"ProjectionType" },
    { CARTTAG_PROJ_XPOS,	     1, 1, TIFF_LONG,	    FIELD_PROJ_XPOS,
	  TRUE,	"Bench X Position" },
    { CARTTAG_PROJ_YPOS,	     1, 1, TIFF_LONG,	    FIELD_PROJ_YPOS,
	  TRUE,	"Bench Y Position" },
    { CARTTAG_LATITUDE,	         1, 1, TIFF_LONG,	    FIELD_LATITUDE,
	  TRUE,	"Bench Latitude" },
    { CARTTAG_LONGITUDE,	     1, 1, TIFF_LONG,	    FIELD_LONGITUDE,
	  TRUE,	"Bench X Longitude" },
    { CARTTAG_XPIXPERANGLE,	     1, 1, TIFF_RATIONAL,	FIELD_XPIXPERANGLE,
	  TRUE,	"X Pixel Per Angle" },
    { CARTTAG_YPIXPERANGLE,	     1, 1, TIFF_RATIONAL,	FIELD_YPIXPERANGLE,
	  TRUE,	"Y Pixel Per Angle" },
    { 0 }
};

#endif


const int tiffDataWidth[] = {
    1,	/* nothing */
    1,	/* TIFF_BYTE */
    1,	/* TIFF_ASCII */
    2,	/* TIFF_SHORT */
    4,	/* TIFF_LONG */
    8,	/* TIFF_RATIONAL */
    1,	/* TIFF_SBYTE */
    1,	/* TIFF_UNDEFINED */
    2,	/* TIFF_SSHORT */
    4,	/* TIFF_SLONG */
    8,	/* TIFF_SRATIONAL */
    4,	/* TIFF_FLOAT */
    8,	/* TIFF_DOUBLE */
};

TIFFFieldInfo const *
DECLARE2(TIFFFindFieldInfo, u_short, tag, TIFFDataType, dt)
{
	static TIFFFieldInfo const *last = NULL;
	register TIFFFieldInfo const *fip;

	if (last && last->field_tag == tag &&
	    (dt == TIFF_ANY || dt == last->field_type))
		return (last);
	/* NB: if table gets big, use sorted search (e.g. binary search) */
	for (fip = tiffFieldInfo; fip->field_tag; fip++)
		if (fip->field_tag == tag &&
		    (dt == TIFF_ANY || fip->field_type == dt))
			return (last = fip);
#ifdef CARTOGRAPHIC_SUPPORT
	/*
	 * Add search for private Carto tag fields so that
	 * standard routines like TIFFSetField will work.
	 */
	for (fip = cartFieldInfo; fip->field_tag; fip++)
		if (fip->field_tag == tag &&
		    (dt == TIFF_ANY || fip->field_type == dt))
			return (last = fip);
#endif
	return ((TIFFFieldInfo *)0);
}

TIFFFieldInfo const *
DECLARE1(TIFFFieldWithTag, u_short, tag)
{
	TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);
	if (fip)
		return (fip);
	TIFFError("TIFFFieldWithTag", "Internal error, unknown tag 0x%x", tag);
	exit(-1);
	/*NOTREACHED*/
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_dump.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_dumpmode.c,v 1.23 92/02/10 19:06:35 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * "Null" Compression Algorithm Support.
 */
#include "tiffiop.h"
#include <stdio.h>
#include <assert.h>
#include <strings.h>

#if USE_PROTOTYPES
static	int DumpModeEncode(TIFF *, u_char *, tiff_u_long, u_int);
static	int DumpModeDecode(TIFF *, u_char *, tiff_u_long, u_int);
static	int DumpModeSeek(TIFF *, int);
extern	int TIFFInitDumpMode(TIFF*);
#else
static	int DumpModeEncode(), DumpModeDecode(), DumpModeSeek();
extern	int TIFFInitDumpMode();
#endif

/*
 * Initialize dump mode.
 */
TIFFInitDumpMode(tif)
	register TIFF *tif;
{
	tif->tif_decoderow = DumpModeDecode;
	tif->tif_decodestrip = DumpModeDecode;
	tif->tif_decodetile = DumpModeDecode;
	tif->tif_encoderow = DumpModeEncode;
	tif->tif_encodestrip = DumpModeEncode;
	tif->tif_encodetile = DumpModeEncode;
	tif->tif_seek = DumpModeSeek;
	return (1);
}

/*
 * Encode a hunk of pixels.
 */
static int
DumpModeEncode(tif, pp, cc, s)
	register TIFF *tif;
	u_char *pp;
	tiff_u_long cc;
	u_int s;
{
	/*
	 * This may be overzealous, but avoids having to
	 * worry about byte alignment for the (potential)
	 * byte-swapping work below.
	 */
	if (tif->tif_rawcc + cc > tif->tif_rawdatasize)
		if (!TIFFFlushData1(tif))
			return (0);
	while (cc > 0) {
		long n;
		if ((n = cc) > tif->tif_rawdatasize)
			n = tif->tif_rawdatasize;
		mybcopy(pp, tif->tif_rawcp, n);
		if (tif->tif_flags & TIFF_SWAB) {
			switch (tif->tif_dir.td_bitspersample) {
			case 16:
				assert((n & 3) == 0);
				TIFFSwabArrayOfShort((u_short *)tif->tif_rawcp,
				    n/2);
				break;
			case 32:
				assert((n & 15) == 0);
				TIFFSwabArrayOfLong((tiff_u_long *)tif->tif_rawcp,
				    n/4);
				break;
			}
		}
		tif->tif_rawcp += n;
		tif->tif_rawcc += n;
		pp += n;
		cc -= n;
		if (tif->tif_rawcc >= tif->tif_rawdatasize &&
		    !TIFFFlushData1(tif))
			return (0);
	}
	return (1);
}

/*
 * Decode a hunk of pixels.
 */
static int
DumpModeDecode(tif, buf, cc, s)
	register TIFF *tif;
	u_char *buf;
	tiff_u_long cc;
	u_int s;
{
	if (tif->tif_rawcc < cc) {
		TIFFError(tif->tif_name,
		    "DumpModeDecode: Not enough data for scanline %d",
		    tif->tif_row);
		return (0);
	}
	/*
	 * Avoid copy if client has setup raw
	 * data buffer to avoid extra copy.
	 */
	if (tif->tif_rawcp != (char*) buf)
		mybcopy(tif->tif_rawcp, buf, cc);
	if (tif->tif_flags & TIFF_SWAB) {
		switch (tif->tif_dir.td_bitspersample) {
		case 16:
			assert((cc & 3) == 0);
			TIFFSwabArrayOfShort((u_short *)buf, cc/2);
			break;
		case 32:
			assert((cc & 15) == 0);
			TIFFSwabArrayOfLong((tiff_u_long *)buf, cc/4);
			break;
		}
	}
	tif->tif_rawcp += cc;
	tif->tif_rawcc -= cc;
	return (1);
}

/*
 * Seek forwards nrows in the current strip.
 */
static int
DumpModeSeek(tif, nrows)
	register TIFF *tif;
	int nrows;
{
	tif->tif_rawcp += nrows * tif->tif_scanlinesize;
	tif->tif_rawcc -= nrows * tif->tif_scanlinesize;
	return (1);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_err.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_error.c,v 1.13 92/02/10 19:06:34 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <stdio.h>

static void
DECLARE3(defaultEHandler, char*, module, char*, fmt, va_list, ap)
{
	if (module != NULL)
		fprintf(stderr, "%s: ", module);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, ".\n");
}

static TIFFErrorHandler _errorHandler = defaultEHandler;

TIFFErrorHandler
DECLARE1(TIFFSetErrorHandler, TIFFErrorHandler, handler)
{
	TIFFErrorHandler prev = _errorHandler;
	_errorHandler = handler;
	return (prev);
}

void
#if USE_PROTOTYPES
TIFFError(char *module, char *fmt, ...)
#else
/*VARARGS2*/
TIFFError(module, fmt, va_alist)
	char *module;
	char *fmt;
	va_dcl
#endif
{
	if (_errorHandler) {
		va_list ap;
		VA_START(ap, fmt);
		(*_errorHandler)(module, fmt, ap);
		va_end(ap);
	}
}



static void
DECLARE3(defaultWHandler, char*, module, char*, fmt, va_list, ap)
{
	if (module != NULL)
		fprintf(stderr, "%s: ", module);
	fprintf(stderr, "Warning, ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, ".\n");
}

static TIFFErrorHandler _warningHandler = defaultWHandler;

TIFFErrorHandler
DECLARE1(TIFFSetWarningHandler, TIFFErrorHandler, handler)
{
	TIFFErrorHandler prev = _warningHandler;
	_warningHandler = handler;
	return (prev);
}

void
#if USE_PROTOTYPES
TIFFWarning(char *module, char *fmt, ...)
#else
/*VARARGS2*/
TIFFWarning(module, fmt, va_alist)
	char *module;
	char *fmt;
	va_dcl
#endif
{
	if (_warningHandler) {
		va_list ap;
		VA_START(ap, fmt);
		(*_warningHandler)(module, fmt, ap);
		va_end(ap);
	}
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_file.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_write.c,v 1.41 92/02/10 19:06:47 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * Scanline-oriented Write Support
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <strings.h>

#define	STRIPINCR	20		/* expansion factor on strip array */

#if USE_PROTOTYPES
static	TIFFWriteCheck(TIFF *, int, char []);
static	TIFFBufferSetup(TIFF *, char []);
static	TIFFGrowStrips(TIFF *, int, char []);
static	TIFFAppendToStrip(TIFF *, u_int, u_char *, tiff_u_long);
extern	int TIFFFlushData1(register TIFF *);
#else
static	TIFFWriteCheck();
static	TIFFBufferSetup();
static	TIFFGrowStrips();
static	TIFFAppendToStrip();
extern	int TIFFFlushData1();
#endif

/** tif_flush **/
#if USE_PROTOTYPES
extern int TIFFFlushData1(TIFF*);
#else
extern int TIFFFlushData1();
#endif

/** end tif_flush **/


/**  tif_read **/


#if USE_PROTOTYPES
static	TIFFSeek(TIFF *, u_int, u_int);
static	long TIFFReadRawStrip1(TIFF *, u_int, u_char *, tiff_u_long, char []);
static	long TIFFReadRawTile1(TIFF *, u_int, u_char *, tiff_u_long, char []);
static	TIFFFillStrip(TIFF *, u_int);
static	TIFFFillTile(TIFF *, u_int);
static	TIFFStartStrip(TIFF *, u_int);
static	TIFFStartTile(TIFF *, u_int);
static	TIFFCheckRead(TIFF *, int);
#else
static	TIFFSeek();
static	long TIFFReadRawStrip1();
static	long TIFFReadRawTile1();
static	TIFFFillStrip();
static	TIFFFillTile();
static	TIFFStartStrip();
static	TIFFStartTile();
static	TIFFCheckRead();
#endif

/** end tif_read **/

/*VARARGS3*/
TIFFWriteScanline(tif, buf, row, sample)
	register TIFF *tif;
	u_char *buf;
	u_int row, sample;
{
	static char module[] = "TIFFWriteScanline";
	register TIFFDirectory *td;
	int strip, status, imagegrew = 0;

	if (!TIFFWriteCheck(tif, 0, module))
		return (-1);
	/*
	 * Handle delayed allocation of data buffer.  This
	 * permits it to be sized more intelligently (using
	 * directory information).
	 */
	if ((tif->tif_flags & TIFF_BUFFERSETUP) == 0) {
		if (!TIFFBufferSetup(tif, module))
			return (-1);
		tif->tif_flags |= TIFF_BUFFERSETUP;
	}
	td = &tif->tif_dir;
	/*
	 * Extend image length if needed
	 * (but only for PlanarConfig=1).
	 */
	if (row >= td->td_imagelength) {	/* extend image */
		if (td->td_planarconfig == PLANARCONFIG_SEPARATE) {
			TIFFError(tif->tif_name,
		"Can not change \"ImageLength\" when using separate planes");
			return (-1);
		}
		td->td_imagelength = row+1;
		imagegrew = 1;
	}
	/*
	 * Calculate strip and check for crossings.
	 */
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE) {
		if (sample >= td->td_samplesperpixel) {
			TIFFError(tif->tif_name,
			    "%d: Sample out of range, max %d",
			    sample, td->td_samplesperpixel);
			return (-1);
		}
		strip = sample*td->td_stripsperimage + row/td->td_rowsperstrip;
	} else
		strip = row / td->td_rowsperstrip;
	if (strip != tif->tif_curstrip) {
		/*
		 * Changing strips -- flush any data present.
		 */
		if (tif->tif_rawcc > 0 && !TIFFFlushData(tif))
			return (-1);
		tif->tif_curstrip = strip;
		/*
		 * Watch out for a growing image.  The value of
		 * strips/image will initially be 1 (since it
		 * can't be deduced until the imagelength is known).
		 */
		if (strip >= td->td_stripsperimage && imagegrew)
			td->td_stripsperimage =
			    howmany(td->td_imagelength, td->td_rowsperstrip);
		tif->tif_row =
		    (strip % td->td_stripsperimage) * td->td_rowsperstrip;
		if (tif->tif_preencode && !(*tif->tif_preencode)(tif))
			return (-1);
		tif->tif_flags |= TIFF_POSTENCODE;
	}
	/*
	 * Check strip array to make sure there's space.
	 * We don't support dynamically growing files that
	 * have data organized in separate bitplanes because
	 * it's too painful.  In that case we require that
	 * the imagelength be set properly before the first
	 * write (so that the strips array will be fully
	 * allocated above).
	 */
	if (strip >= td->td_nstrips && !TIFFGrowStrips(tif, 1, module))
		return (-1);
	/*
	 * Ensure the write is either sequential or at the
	 * beginning of a strip (or that we can randomly
	 * access the data -- i.e. no encoding).
	 */
	if (row != tif->tif_row) {
		if (tif->tif_seek) {
			if (row < tif->tif_row) {
				/*
				 * Moving backwards within the same strip:
				 * backup to the start and then decode
				 * forward (below).
				 */
				tif->tif_row = (strip % td->td_stripsperimage) *
				    td->td_rowsperstrip;
				tif->tif_rawcp = tif->tif_rawdata;
			}
			/*
			 * Seek forward to the desired row.
			 */
			if (!(*tif->tif_seek)(tif, row - tif->tif_row))
				return (-1);
			tif->tif_row = row;
		} else {
			TIFFError(tif->tif_name,
		    "Compression algorithm does not support random access");
			return (-1);
		}
	}
	status = (*tif->tif_encoderow)(tif, buf, tif->tif_scanlinesize, sample);
	tif->tif_row++;
	return (status);
}

/*
 * Encode the supplied data and write it to the
 * specified strip.  There must be space for the
 * data; we don't check if strips overlap!
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteEncodedStrip(tif, strip, data, cc)
	TIFF *tif;
	u_int strip;
	u_char *data;
	tiff_u_long cc;
{
	static char module[] = "TIFFWriteEncodedStrip";
	TIFFDirectory *td = &tif->tif_dir;

	if (!TIFFWriteCheck(tif, 0, module))
		return (-1);
	if (strip >= td->td_nstrips) {
		TIFFError(module, "%s: Strip %d out of range, max %d",
		    tif->tif_name, strip, td->td_nstrips);
		return (-1);
	}
	/*
	 * Handle delayed allocation of data buffer.  This
	 * permits it to be sized according to the directory
	 * info.
	 */
	if ((tif->tif_flags & TIFF_BUFFERSETUP) == 0) {
		if (!TIFFBufferSetup(tif, module))
			return (-1);
		tif->tif_flags |= TIFF_BUFFERSETUP;
	}
	tif->tif_curstrip = strip;
	tif->tif_flags &= ~TIFF_POSTENCODE;
	if (tif->tif_preencode && !(*tif->tif_preencode)(tif))
		return (-1);
	if (!(*tif->tif_encodestrip)(tif,
	    data, cc, strip / td->td_stripsperimage))
		return (0);
	if (tif->tif_postencode && !(*tif->tif_postencode)(tif))
		return (-1);
	if (td->td_fillorder != tif->tif_fillorder &&
	    (tif->tif_flags & TIFF_NOBITREV) == 0)
		TIFFReverseBits((u_char *)tif->tif_rawdata, tif->tif_rawcc);
	if (tif->tif_rawcc > 0 &&
	    !TIFFAppendToStrip(tif, strip, (u_char *)tif->tif_rawdata, tif->tif_rawcc))
		return (-1);
	tif->tif_rawcc = 0;
	tif->tif_rawcp = tif->tif_rawdata;
	return (cc);
}

/*
 * Write the supplied data to the specified strip.
 * There must be space for the data; we don't check
 * if strips overlap!
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteRawStrip(tif, strip, data, cc)
	TIFF *tif;
	u_int strip;
	u_char *data;
	tiff_u_long cc;
{
	static char module[] = "TIFFWriteRawStrip";

	if (!TIFFWriteCheck(tif, 0, module))
		return (-1);
	if (strip >= tif->tif_dir.td_nstrips) {
		TIFFError(module, "%s: Strip %d out of range, max %d",
		    tif->tif_name, strip, tif->tif_dir.td_nstrips);
		return (-1);
	}
	return (TIFFAppendToStrip(tif, strip, data, cc) ? cc : -1);
}

/*
 * Write and compress a tile of data.  The
 * tile is selected by the (x,y,z,s) coordinates.
 */
TIFFWriteTile(tif, buf, x, y, z, s)
	TIFF *tif;
	u_char *buf;
	tiff_u_long x, y, z;
	u_int s;
{
	if (!TIFFCheckTile(tif, x, y, z, s))
		return (-1);
	/*
	 * NB: A tile size of -1 is used instead of tif_tilesize knowing
	 *     that TIFFWriteEncodedTile will clamp this to the tile size.
	 *     This is done because the tile size may not be defined until
	 *     after the output buffer is setup in TIFFBufferSetup.
	 */
	return (TIFFWriteEncodedTile(tif,
	    TIFFComputeTile(tif, x, y, z, s), buf, (tiff_u_long)-1));
}

/*
 * Encode the supplied data and write it to the
 * specified tile.  There must be space for the
 * data.  The function clamps individual writes
 * to a tile to the tile size, but does not (and
 * can not) check that multiple writes to the same
 * tile do not write more than tile size data.
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteEncodedTile(tif, tile, data, cc)
	TIFF *tif;
	u_int tile;
	u_char *data;
	tiff_u_long cc;
{
	static char module[] = "TIFFWriteEncodedTile";
	TIFFDirectory *td;

	if (!TIFFWriteCheck(tif, 1, module))
		return (-1);
	td = &tif->tif_dir;
	if (tile >= td->td_nstrips) {
		TIFFError(module, "%s: Tile %d out of range, max %d",
		    tif->tif_name, tile, td->td_nstrips);
		return (-1);
	}
	/*
	 * Handle delayed allocation of data buffer.  This
	 * permits it to be sized more intelligently (using
	 * directory information).
	 */
	if ((tif->tif_flags & TIFF_BUFFERSETUP) == 0) {
		if (!TIFFBufferSetup(tif, module))
			return (-1);
		tif->tif_flags |= TIFF_BUFFERSETUP;
	}
	tif->tif_curtile = tile;
	/* 
	 * Compute tiles per row & per column to compute
	 * current row and column
	 */
    /* ndr -- fixed mathematical error in row computation - cf tif_read */
	tif->tif_row = (tile / howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilelength;
	tif->tif_col = (tile % howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilewidth;

	tif->tif_flags &= ~TIFF_POSTENCODE;
	if (tif->tif_preencode && !(*tif->tif_preencode)(tif))
		return (-1);
	/*
	 * Clamp write amount to the tile size.  This is mostly
	 * done so that callers can pass in some large number
	 * (e.g. -1) and have the tile size used instead.
	 */
	if (cc > tif->tif_tilesize)
		cc = tif->tif_tilesize;
	if (!(*tif->tif_encodetile)(tif, data, cc, tile/td->td_stripsperimage))
		return (0);
	if (tif->tif_postencode && !(*tif->tif_postencode)(tif))
		return (-1);
	if (td->td_fillorder != tif->tif_fillorder &&
	    (tif->tif_flags & TIFF_NOBITREV) == 0)
		TIFFReverseBits((u_char *)tif->tif_rawdata, tif->tif_rawcc);
	if (tif->tif_rawcc > 0 && !TIFFAppendToStrip(tif, tile,
	    (u_char *)tif->tif_rawdata, tif->tif_rawcc))
		return (-1);
	tif->tif_rawcc = 0;
	tif->tif_rawcp = tif->tif_rawdata;
	return (cc);
}

/*
 * Write the supplied data to the specified strip.
 * There must be space for the data; we don't check
 * if strips overlap!
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteRawTile(tif, tile, data, cc)
	TIFF *tif;
	u_int tile;
	u_char *data;
	tiff_u_long cc;
{
	static char module[] = "TIFFWriteRawTile";

	if (!TIFFWriteCheck(tif, 1, module))
		return (-1);
	if (tile >= tif->tif_dir.td_nstrips) {
		TIFFError(module, "%s: Tile %d out of range, max %d",
		    tif->tif_name, tile, tif->tif_dir.td_nstrips);
		return (-1);
	}
	return (TIFFAppendToStrip(tif, tile, data, cc) ? cc : -1);
}

static
TIFFSetupStrips(tif)
	TIFF *tif;
{
#define	isUnspecified(td, v) \
    (td->v == 0xffffffff || (td)->td_imagelength == 0)
	register TIFFDirectory *td = &tif->tif_dir;

	if (!isTiled(tif))
		td->td_stripsperimage = isUnspecified(td, td_rowsperstrip) ?
		    1 : howmany(td->td_imagelength, td->td_rowsperstrip);
	else
		td->td_stripsperimage = isUnspecified(td, td_tilelength) ?
		    1 : TIFFNumberOfTiles(tif);
	td->td_nstrips = td->td_stripsperimage;
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE)
		td->td_nstrips *= td->td_samplesperpixel;
	td->td_stripoffset = (tiff_u_long *)
	    malloc(td->td_nstrips * sizeof (tiff_u_long));
	td->td_stripbytecount = (tiff_u_long *)
	    malloc(td->td_nstrips * sizeof (tiff_u_long));
	if (td->td_stripoffset == NULL || td->td_stripbytecount == NULL)
		return (0);
	/*
	 * Place data at the end-of-file
	 * (by setting offsets to zero).
	 */
	mybzero((char *)td->td_stripoffset, td->td_nstrips * sizeof (tiff_u_long));
	mybzero((char *)td->td_stripbytecount, td->td_nstrips * sizeof (tiff_u_long));
	TIFFSetFieldBit(tif, FIELD_STRIPOFFSETS);
	TIFFSetFieldBit(tif, FIELD_STRIPBYTECOUNTS);
	return (1);
#undef isUnspecified
}

/*
 * Verify file is writable and that the directory
 * information is setup properly.  In doing the latter
 * we also "freeze" the state of the directory so
 * that important information is not changed.
 */
static
TIFFWriteCheck(tif, tiles, module)
	register TIFF *tif;
	int tiles;
	char module[];
{
	if (tif->tif_mode == O_RDONLY) {
		TIFFError(module, "%s: File not open for writing",
		    tif->tif_name);
		return (0);
	}
	if (tiles ^ isTiled(tif)) {
		TIFFError(tif->tif_name, tiles ?
		    "Can not write tiles to a stripped image" :
		    "Can not write scanlines to a tiled image");
		return (0);
	}
	/*
	 * On the first write verify all the required information
	 * has been setup and initialize any data structures that
	 * had to wait until directory information was set.
	 * Note that a lot of our work is assumed to remain valid
	 * because we disallow any of the important parameters
	 * from changing after we start writing (i.e. once
	 * TIFF_BEENWRITING is set, TIFFSetField will only allow
	 * the image's length to be changed).
	 */
	if ((tif->tif_flags & TIFF_BEENWRITING) == 0) {
		if (!TIFFFieldSet(tif, FIELD_IMAGEDIMENSIONS)) {
			TIFFError(module,
			    "%s: Must set \"ImageWidth\" before writing data",
			    tif->tif_name);
			return (0);
		}
		if (!TIFFFieldSet(tif, FIELD_PLANARCONFIG)) {
			TIFFError(module,
		    "%s: Must set \"PlanarConfiguration\" before writing data",
			    tif->tif_name);
			return (0);
		}
		if (tif->tif_dir.td_stripoffset == NULL &&
		    !TIFFSetupStrips(tif)) {
			tif->tif_dir.td_nstrips = 0;
			TIFFError(module, "%s: No space for %s arrays",
			    tif->tif_name, isTiled(tif) ? "tile" : "strip");
			return (0);
		}
		tif->tif_flags |= TIFF_BEENWRITING;
	}
	return (1);
}

/*
 * Setup the raw data buffer used for encoding.
 */
static
TIFFBufferSetup(tif, module)
	register TIFF *tif;
	char module[];
{
	long size;

	if (isTiled(tif))
		tif->tif_tilesize = size = TIFFTileSize(tif);
	else
		tif->tif_scanlinesize = size = TIFFScanlineSize(tif);
	/*
	 * Make raw data buffer at least 8K
	 */
	if (size < 8*1024)
		size = 8*1024;
	tif->tif_rawdata = malloc(size);
	if (tif->tif_rawdata == NULL) {
		TIFFError(module, "%s: No space for output buffer",
		    tif->tif_name);
		return (0);
	}
	tif->tif_rawdatasize = size;
	tif->tif_rawcc = 0;
	tif->tif_rawcp = tif->tif_rawdata;
	return (1);
}

/*
 * Grow the strip data structures by delta strips.
 */
static
TIFFGrowStrips(tif, delta, module)
	TIFF *tif;
	int delta;
	char module[];
{
	TIFFDirectory *td = &tif->tif_dir;

	assert(td->td_planarconfig == PLANARCONFIG_CONTIG);
	td->td_stripoffset = (tiff_u_long *)realloc(td->td_stripoffset,
	    (td->td_nstrips + delta) * sizeof (tiff_u_long));
	td->td_stripbytecount = (tiff_u_long *)realloc(td->td_stripbytecount,
	    (td->td_nstrips + delta) * sizeof (tiff_u_long));
	if (td->td_stripoffset == NULL || td->td_stripbytecount == NULL) {
		td->td_nstrips = 0;
		TIFFError(module, "%s: No space to expand strip arrays",
		    tif->tif_name);
		return (0);
	}
	mybzero(td->td_stripoffset+td->td_nstrips, delta*sizeof (tiff_u_long));
	mybzero(td->td_stripbytecount+td->td_nstrips, delta*sizeof (tiff_u_long));
	td->td_nstrips += delta;
	return (1);
}

/*
 * Append the data to the specified strip.
 *
 * NB: We don't check that there's space in the
 *     file (i.e. that strips do not overlap).
 */
static
TIFFAppendToStrip(tif, strip, data, cc)
	TIFF *tif;
	u_int strip;
	u_char *data;
	tiff_u_long cc;
{
	TIFFDirectory *td = &tif->tif_dir;
	static char module[] = "TIFFAppendToStrip";

	if (td->td_stripoffset[strip] == 0 || tif->tif_curoff == 0) {
		/*
		 * No current offset, set the current strip.
		 */
		if (td->td_stripoffset[strip] != 0) {
			if (!SeekOK(tif->tif_fd, td->td_stripoffset[strip])) {
				TIFFError(module,
				    "%s: Seek error at scanline %d",
				    tif->tif_name, tif->tif_row);
				return (0);
			}
		} else
			td->td_stripoffset[strip] =
			    lseek(tif->tif_fd, 0L, L_XTND);
		tif->tif_curoff = td->td_stripoffset[strip];
	}
	if (!WriteOK(tif->tif_fd, data, cc)) {
		TIFFError(module, "%s: Write error at scanline %d",
		    tif->tif_name, tif->tif_row);
		return (0);
	}
	tif->tif_curoff += cc;
	td->td_stripbytecount[strip] += cc;
	return (1);
}

/*
 * Internal version of TIFFFlushData that can be
 * called by ``encodestrip routines'' w/o concern
 * for infinite recursion.
 */
TIFFFlushData1(tif)
	register TIFF *tif;
{
	if (tif->tif_rawcc > 0) {
		if (tif->tif_dir.td_fillorder != tif->tif_fillorder &&
		    (tif->tif_flags & TIFF_NOBITREV) == 0)
			TIFFReverseBits((u_char *)tif->tif_rawdata,
			    tif->tif_rawcc);
		if (!TIFFAppendToStrip(tif,
		    isTiled(tif) ? tif->tif_curtile : tif->tif_curstrip,
		    (u_char *)tif->tif_rawdata, tif->tif_rawcc))
			return (0);
		tif->tif_rawcc = 0;
		tif->tif_rawcp = tif->tif_rawdata;
	}
	return (1);
}

/* tif_read */


/*VARARGS3*/
TIFFReadScanline(tif, buf, row, sample)
	register TIFF *tif;
	u_char *buf;
	u_int row, sample;
{
	int e;

	if (!TIFFCheckRead(tif, 0))
		return (-1);
	if (e = TIFFSeek(tif, row, sample)) {
		/*
		 * Decompress desired row into user buffer.
		 */
		e = (*tif->tif_decoderow)(tif, buf, tif->tif_scanlinesize, sample);
		tif->tif_row++;
	}
	return (e ? 1 : -1);
}

/*
 * Seek to a random row+sample in a file.
 */
static
/*VARARGS2*/
TIFFSeek(tif, row, sample)
	register TIFF *tif;
	u_int row, sample;
{
	register TIFFDirectory *td = &tif->tif_dir;
	int strip;

	if (row >= td->td_imagelength) {	/* out of range */
		TIFFError(tif->tif_name, "%d: Row out of range, max %d",
		    row, td->td_imagelength);
		return (0);
	}
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE) {
		if (sample >= td->td_samplesperpixel) {
			TIFFError(tif->tif_name,
			    "%d: Sample out of range, max %d",
			    sample, td->td_samplesperpixel);
			return (0);
		}
		strip = sample*td->td_stripsperimage + row/td->td_rowsperstrip;
	} else
		strip = row / td->td_rowsperstrip;
	if (strip != tif->tif_curstrip) { 	/* different strip, refill */
		if (!TIFFFillStrip(tif, strip))
			return (0);
	} else if (row < tif->tif_row) {
		/*
		 * Moving backwards within the same strip: backup
		 * to the start and then decode forward (below).
		 *
		 * NB: If you're planning on lots of random access within a
		 * strip, it's better to just read and decode the entire
		 * strip, and then access the decoded data in a random fashion.
		 */
		if (!TIFFStartStrip(tif, strip))
			return (0);
	}
	if (row != tif->tif_row) {
		if (tif->tif_seek) {
			/*
			 * Seek forward to the desired row.
			 */
			if (!(*tif->tif_seek)(tif, row - tif->tif_row))
				return (0);
			tif->tif_row = row;
		} else {
			TIFFError(tif->tif_name,
		    "Compression algorithm does not support random access");
			return (0);
		}
	}
	return (1);
}

/*
 * Read a strip of data and decompress the specified
 * amount into the user-supplied buffer.
 */
long
TIFFReadEncodedStrip(tif, strip, buf, size)
	TIFF *tif;
	u_int strip;
	u_char *buf;
	tiff_u_long size;
{
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long stripsize = TIFFStripSize(tif);

	if (!TIFFCheckRead(tif, 0))
		return ((tiff_u_long)-1);
	if (strip >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Strip out of range, max %d",
		    strip, td->td_nstrips);
		return (-1);
	}
	/*
	 * Calculate the strip size according to the number of
	 * rows in the strip (check for truncated last strip).
	 */
	if (size == (tiff_u_long)-1)
		size = stripsize;
	else if (size > stripsize)
		size = stripsize;
	return (TIFFFillStrip(tif, strip) && 
    (*tif->tif_decodestrip)(tif, buf, size, strip / td->td_stripsperimage) ?
	    size : -1);
}

/*
 * Read a strip of data from the file.
 */
long
TIFFReadRawStrip(tif, strip, buf, size)
	TIFF *tif;
	u_int strip;
	u_char *buf;
	tiff_u_long size;
{
	static char module[] = "TIFFReadRawStrip";
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long bytecount;

	if (!TIFFCheckRead(tif, 0))
		return (-1);
	if (strip >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Strip out of range, max %d",
		    strip, td->td_nstrips);
		return (-1);
	}
	bytecount = td->td_stripbytecount[strip];
	if (size != (u_int)-1 && size < bytecount)
		bytecount = size;
	return (TIFFReadRawStrip1(tif, strip, buf, bytecount, module));
}

static long
TIFFReadRawStrip1(tif, strip, buf, size, module)
	TIFF *tif;
	u_int strip;
	u_char *buf;
	tiff_u_long size;
	char module[];
{
	TIFFDirectory *td = &tif->tif_dir;

	if (!isMapped(tif)) {
		if (!SeekOK(tif->tif_fd, td->td_stripoffset[strip])) {
			TIFFError(module,
			    "%s: Seek error at scanline %d, strip %d",
			    tif->tif_name, tif->tif_row, strip);
			return (-1);
		}
		if (!ReadOK(tif->tif_fd, buf, size)) {
			TIFFError(module, "%s: Read error at scanline %d",
			    tif->tif_name, tif->tif_row);
			return (-1);
		}
#ifdef MMAP_SUPPORT
	} else {
		if (td->td_stripoffset[strip] + size > tif->tif_size) {
			TIFFError(module,
			    "%s: Seek error at scanline %d, strip %d",
			    tif->tif_name, tif->tif_row, strip);
			return (-1);
		}
		mybcopy(tif->tif_base + td->td_stripoffset[strip], buf, size);
#endif
	}
	return (size);
}

/*
 * Read the specified strip and setup for decoding. 
 * The data buffer is expanded, as necessary, to
 * hold the strip's data.
 */
static
TIFFFillStrip(tif, strip)
	TIFF *tif;
	u_int strip;
{
	static char module[] = "TIFFFillStrip";
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long bytecount;

	bytecount = td->td_stripbytecount[strip];
#ifdef MMAP_SUPPORT
	if (isMapped(tif) &&
	    (td->td_fillorder == tif->tif_fillorder || (tif->tif_flags & TIFF_NOBITREV))) {
		/*
		 * The image is mapped into memory and we either don't
		 * need to flip bits or the compression routine is going
		 * to handle this operation itself.  In this case, avoid
		 * copying the raw data and instead just reference the
		 * data from the memory mapped file image.  This assumes
		 * that the decompression routines do not modify the
		 * contents of the raw data buffer (if they try to,
		 * the application will get a fault since the file is
		 * mapped read-only).
		 */
		if ((tif->tif_flags & TIFF_MYBUFFER) && tif->tif_rawdata)
			free(tif->tif_rawdata);
		tif->tif_flags &= ~TIFF_MYBUFFER;
		if (td->td_stripoffset[strip] + bytecount > tif->tif_size) {
			/*
			 * This error message might seem strange, but it's
			 * what would happen if a read were done instead.
			 */
			TIFFError(module, "%s: Read error on strip %d",
			    tif->tif_name, strip);
			tif->tif_curstrip = -1;		/* unknown state */
			return (0);
		}
		tif->tif_rawdatasize = bytecount;
		tif->tif_rawdata = tif->tif_base + td->td_stripoffset[strip];
	} else {
#endif
		/*
		 * Expand raw data buffer, if needed, to
		 * hold data strip coming from file
		 * (perhaps should set upper bound on
		 *  the size of a buffer we'll use?).
		 */
		if (bytecount > tif->tif_rawdatasize) {
			tif->tif_curstrip = -1;		/* unknown state */
			if ((tif->tif_flags & TIFF_MYBUFFER) == 0) {
				TIFFError(module,
				"%s: Data buffer too small to hold strip %d",
				    tif->tif_name, strip);
				return (0);
			}
			if (!TIFFReadBufferSetup(tif, 0,
			    roundup(bytecount, 1024)))
				return (0);
		}
		if (TIFFReadRawStrip1(tif, strip, (u_char *)tif->tif_rawdata,
		    bytecount, module) != bytecount)
			return (0);
		if (td->td_fillorder != tif->tif_fillorder &&
		    (tif->tif_flags & TIFF_NOBITREV) == 0)
			TIFFReverseBits((u_char *)tif->tif_rawdata, bytecount);
#ifdef MMAP_SUPPORT
	}
#endif
	return (TIFFStartStrip(tif, strip));
}

/*
 * Tile-oriented Read Support
 * Contributed by Nancy Cam (Silicon Graphics).
 */

/*
 * Read and decompress a tile of data.  The
 * tile is selected by the (x,y,z,s) coordinates.
 */
TIFFReadTile(tif, buf, x, y, z, s)
	TIFF *tif;
	u_char *buf;
	tiff_u_long x, y, z;
	u_int s;
{
	u_int tile;

	if (!TIFFCheckRead(tif, 1) || !TIFFCheckTile(tif, x, y, z, s))
		return (-1);
	tile = TIFFComputeTile(tif, x, y, z, s);
	if (tile >= tif->tif_dir.td_nstrips) {
		TIFFError(tif->tif_name, "%d: Tile out of range, max %d",
		    tile, tif->tif_dir.td_nstrips);
		return (-1);
	}
	return (TIFFFillTile(tif, tile) &&
	    (*tif->tif_decodetile)(tif, buf, tif->tif_tilesize, s) ?
	    tif->tif_tilesize : -1);
}

/*
 * Read a tile of data and decompress the specified
 * amount into the user-supplied buffer.
 */
long
TIFFReadEncodedTile(tif, tile, buf, size)
	TIFF *tif;
	u_int tile;
	u_char *buf;
	tiff_u_long size;
{
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long tilesize = tif->tif_tilesize;

	if (!TIFFCheckRead(tif, 1))
		return (-1);
	if (tile >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Tile out of range, max %d",
		    tile, td->td_nstrips);
		return (-1);
	}
	if (size == (tiff_u_long)-1)
		size = tilesize;
	else if (size > tilesize )
		size = tilesize;
	return (TIFFFillTile(tif, tile) && 
	    (*tif->tif_decodetile)(tif, buf, size, tile/td->td_stripsperimage) ?
	    size : -1);
}

/*
 * Read a tile of data from the file.
 */
long
TIFFReadRawTile(tif, tile, buf, size)
	TIFF *tif;
	u_int tile;
	u_char *buf;
	tiff_u_long size;
{
	static char module[] = "TIFFReadRawTile";
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long bytecount;

	if (!TIFFCheckRead(tif, 1))
		return (-1);
	if (tile >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Tile out of range, max %d",
		    tile, td->td_nstrips);
		return (-1);
	}
	bytecount = td->td_stripbytecount[tile];
	if (size != (tiff_u_long)-1 && size < bytecount)
		bytecount = size;
	return (TIFFReadRawTile1(tif, tile, buf, bytecount, module));
}

static long
TIFFReadRawTile1(tif, tile, buf, size, module)
	TIFF *tif;
	u_int tile;
	u_char *buf;
	tiff_u_long size;
	char module[];
{
	TIFFDirectory *td = &tif->tif_dir;

	if (!isMapped(tif)) {
		if (!SeekOK(tif->tif_fd, td->td_stripoffset[tile])) {
			TIFFError(module,
			    "%s: Seek error at row %d, col %d, tile %d",
			    tif->tif_name, tif->tif_row, tif->tif_col, tile);
			return (-1);
		}
		if (!ReadOK(tif->tif_fd, buf, size)) {
			TIFFError(module, "%s: Read error at row %d, col %d",
			    tif->tif_name, tif->tif_row, tif->tif_col);
			return (-1);
		}
#ifdef MMAP_SUPPORT
	} else {
		if (td->td_stripoffset[tile] + size > tif->tif_size) {
			TIFFError(module,
			    "%s: Seek error at row %d, col %d, tile %d",
			    tif->tif_name, tif->tif_row, tif->tif_col, tile);
			return (-1);
		}
		mybcopy(tif->tif_base + td->td_stripoffset[tile], buf, size);
#endif
	}
	return (size);
}

/*
 * Read the specified tile and setup for decoding. 
 * The data buffer is expanded, as necessary, to
 * hold the tile's data.
 */
static
TIFFFillTile(tif, tile)
	TIFF *tif;
	u_int tile;
{
	static char module[] = "TIFFFillTile";
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long bytecount;

	bytecount = td->td_stripbytecount[tile];
#ifdef MMAP_SUPPORT
	if (isMapped(tif) &&
	    (td->td_fillorder == tif->tif_fillorder || (tif->tif_flags & TIFF_NOBITREV))) {
		/*
		 * The image is mapped into memory and we either don't
		 * need to flip bits or the compression routine is going
		 * to handle this operation itself.  In this case, avoid
		 * copying the raw data and instead just reference the
		 * data from the memory mapped file image.  This assumes
		 * that the decompression routines do not modify the
		 * contents of the raw data buffer (if they try to,
		 * the application will get a fault since the file is
		 * mapped read-only).
		 */
		if ((tif->tif_flags & TIFF_MYBUFFER) && tif->tif_rawdata)
			free(tif->tif_rawdata);
		tif->tif_flags &= ~TIFF_MYBUFFER;
		if (td->td_stripoffset[tile] + bytecount > tif->tif_size) {
			tif->tif_curtile = -1;		/* unknown state */
			return (0);
		}
		tif->tif_rawdatasize = bytecount;
		tif->tif_rawdata = tif->tif_base + td->td_stripoffset[tile];
	} else {
#endif
		/*
		 * Expand raw data buffer, if needed, to
		 * hold data tile coming from file
		 * (perhaps should set upper bound on
		 *  the size of a buffer we'll use?).
		 */
		if (bytecount > tif->tif_rawdatasize) {
			tif->tif_curtile = -1;		/* unknown state */
			if ((tif->tif_flags & TIFF_MYBUFFER) == 0) {
				TIFFError(module,
				"%s: Data buffer too small to hold tile %d",
				    tif->tif_name, tile);
				return (0);
			}
			if (!TIFFReadBufferSetup(tif, 0,
			    roundup(bytecount, 1024)))
				return (0);
		}
		if (TIFFReadRawTile1(tif, tile, (u_char *)tif->tif_rawdata,
		    bytecount, module) != bytecount)
			return (0);
		if (td->td_fillorder != tif->tif_fillorder &&
		    (tif->tif_flags & TIFF_NOBITREV) == 0)
			TIFFReverseBits((u_char *)tif->tif_rawdata, bytecount);
#ifdef MMAP_SUPPORT
	}
#endif
	return (TIFFStartTile(tif, tile));
}

/*
 * Setup the raw data buffer in preparation for
 * reading a strip of raw data.  If the buffer
 * is specified as zero, then a buffer of appropriate
 * size is allocated by the library.  Otherwise,
 * the client must guarantee that the buffer is
 * large enough to hold any individual strip of
 * raw data.
 */
int
TIFFReadBufferSetup(tif, bp, size)
	TIFF *tif;
	char *bp;
	tiff_u_long size;
{
	static char module[] = "TIFFReadBufferSetup";

	if (tif->tif_rawdata) {
		if (tif->tif_flags & TIFF_MYBUFFER)
			free(tif->tif_rawdata);
		tif->tif_rawdata = NULL;
	}
	if (bp) {
		tif->tif_rawdatasize = size;
		tif->tif_rawdata = bp;
		tif->tif_flags &= ~TIFF_MYBUFFER;
	} else {
		tif->tif_rawdatasize = roundup(size, 1024);
		tif->tif_rawdata = malloc(tif->tif_rawdatasize);
		tif->tif_flags |= TIFF_MYBUFFER;
	}
	if (tif->tif_rawdata == NULL) {
		TIFFError(module,
		    "%s: No space for data buffer at scanline %d",
		    tif->tif_name, tif->tif_row);
		tif->tif_rawdatasize = 0;
		return (0);
	}
	return (1);
}

/*
 * Set state to appear as if a
 * strip has just been read in.
 */
static
TIFFStartStrip(tif, strip)
	register TIFF *tif;
	u_int strip;
{
	TIFFDirectory *td = &tif->tif_dir;

	tif->tif_curstrip = strip;
	tif->tif_row = (strip % td->td_stripsperimage) * td->td_rowsperstrip;
	tif->tif_rawcp = tif->tif_rawdata;
	tif->tif_rawcc = td->td_stripbytecount[strip];
	return (tif->tif_predecode == NULL || (*tif->tif_predecode)(tif));
}

/*
 * Set state to appear as if a
 * tile has just been read in.
 */
static
TIFFStartTile(tif, tile)
	register TIFF *tif;
	u_int tile;
{
	TIFFDirectory *td = &tif->tif_dir;

	tif->tif_curtile = tile;
    /* ndr -- fixed mathematical error in row computation -cf tif_write */
	tif->tif_row = (tile / howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilelength;
	tif->tif_col = (tile % howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilewidth;
	tif->tif_rawcp = tif->tif_rawdata;
	tif->tif_rawcc = td->td_stripbytecount[tile];
	return (tif->tif_predecode == NULL || (*tif->tif_predecode)(tif));
}

static
TIFFCheckRead(tif, tiles)
	TIFF *tif;
	int tiles;
{
	if (tif->tif_mode == O_WRONLY) {
		TIFFError(tif->tif_name, "File not open for reading");
		return (0);
	}
	if (tiles ^ isTiled(tif)) {
		TIFFError(tif->tif_name, tiles ?
		    "Can not read tiles from a stripped image" :
		    "Can not read scanlines from a tiled image");
		return (0);
	}
	return (1);
}

/* end tif_read */

/* tif_flush */
TIFFFlush(tif)
	TIFF *tif;
{

	if (tif->tif_mode != O_RDONLY) {
		if (!TIFFFlushData(tif))
			return (0);
		if ((tif->tif_flags & TIFF_DIRTYDIRECT) &&
		    !TIFFWriteDirectory(tif))
			return (0);
	}
	return (1);
}

/*
 * Flush buffered data to the file.
 */
TIFFFlushData(tif)
	TIFF *tif;
{
	if ((tif->tif_flags & TIFF_BEENWRITING) == 0)
		return (0);
	if (tif->tif_flags & TIFF_POSTENCODE) {
		tif->tif_flags &= ~TIFF_POSTENCODE;
		if (tif->tif_postencode && !(*tif->tif_postencode)(tif))
			return (0);
	}
	return (TIFFFlushData1(tif));
}
/* end tif_flush */


#if USE_PROTOTYPES
extern	int TIFFDefaultDirectory(TIFF*);
static int TIFFReadTIFFHdr( int fd, TIFFHeader *hdr );
#else
extern	int TIFFDefaultDirectory();
static int TIFFReadTIFFHdr();
#endif

static const long typemask[13] = {
	0,		/* TIFF_NOTYPE */
	0x000000ff,	/* TIFF_BYTE */
	0xffffffff,	/* TIFF_ASCII */
	0x0000ffff,	/* TIFF_SHORT */
	0xffffffff,	/* TIFF_LONG */
	0xffffffff,	/* TIFF_RATIONAL */
	0x000000ff,	/* TIFF_SBYTE */
	0x000000ff,	/* TIFF_UNDEFINED */
	0x0000ffff,	/* TIFF_SSHORT */
	0xffffffff,	/* TIFF_SLONG */
	0xffffffff,	/* TIFF_SRATIONAL */
	0xffffffff,	/* TIFF_FLOAT */
	0xffffffff,	/* TIFF_DOUBLE */
};
static const int bigTypeshift[13] = {
	0,		/* TIFF_NOTYPE */
	24,		/* TIFF_BYTE */
	0,		/* TIFF_ASCII */
	16,		/* TIFF_SHORT */
	0,		/* TIFF_LONG */
	0,		/* TIFF_RATIONAL */
	16,		/* TIFF_SBYTE */
	16,		/* TIFF_UNDEFINED */
	24,		/* TIFF_SSHORT */
	0,		/* TIFF_SLONG */
	0,		/* TIFF_SRATIONAL */
	0,		/* TIFF_FLOAT */
	0,		/* TIFF_DOUBLE */
};
static const int litTypeshift[13] = {
	0,		/* TIFF_NOTYPE */
	0,		/* TIFF_BYTE */
	0,		/* TIFF_ASCII */
	0,		/* TIFF_SHORT */
	0,		/* TIFF_LONG */
	0,		/* TIFF_RATIONAL */
	0,		/* TIFF_SBYTE */
	0,		/* TIFF_UNDEFINED */
	0,		/* TIFF_SSHORT */
	0,		/* TIFF_SLONG */
	0,		/* TIFF_SRATIONAL */
	0,		/* TIFF_FLOAT */
	0,		/* TIFF_DOUBLE */
};


/*
 * Initialize the bit fill order, the
 * shift & mask tables, and the byte
 * swapping state according to the file
 * contents and the machine architecture.
 */
static
DECLARE3(TIFFInitOrder, register TIFF*, tif, int, magic, int, bigendian)
{
	/* XXX how can we deduce this dynamically? */
	tif->tif_fillorder = FILLORDER_MSB2LSB;

	tif->tif_typemask = typemask;
	if (magic == TIFF_BIGENDIAN) {
		tif->tif_typeshift = bigTypeshift;
		if (!bigendian)
			tif->tif_flags |= TIFF_SWAB;
	} else {
		tif->tif_typeshift = litTypeshift;
		if (bigendian)
			tif->tif_flags |= TIFF_SWAB;
	}
}

static int
DECLARE2(getMode, char*, mode, char*, module)
{
	int m = -1;

	switch (mode[0]) {
	case 'r':
		m = O_RDONLY;
		if (mode[1] == '+')
			m = O_RDWR;
		break;
	case 'w':
	case 'a':
		m = O_RDWR|O_CREAT;
		if (mode[0] == 'w')
			m |= O_TRUNC;
		break;
	default:
		TIFFError(module, "\"%s\": Bad mode", mode);
		break;
	}
	return (m);
}

/*
 * Open a TIFF file for read/writing.
 */
TIFF *
TIFFOpen(name, mode)
	char *name, *mode;
{
	static char module[] = "TIFFOpen";
	int m, fd;

	m = getMode(mode, module);
	if (m == -1)
		return ((TIFF *)0);
	fd = TIFFOpenFile(name, m, 0666);
	if (fd < 0) {
		TIFFError(module, "%s: Cannot open", name);
		return ((TIFF *)0);
	}
	return (TIFFFdOpen(fd, name, mode));
}

/*
 * Open a TIFF file descriptor for read/writing.
 */
TIFF *
TIFFFdOpen(fd, name, mode)
	int fd;
	char *name, *mode;
{
	static char module[] = "TIFFFdOpen";
	TIFF *tif;
	int m, bigendian;

	m = getMode(mode, module);
	if (m == -1)
		goto bad2;
	tif = (TIFF *)malloc(sizeof (TIFF) + strlen(name) + 1);
	if (tif == NULL) {
		TIFFError(module, "%s: Out of memory (TIFF structure)", name);
		goto bad2;
	}
	mybzero((char *)tif, sizeof (*tif));
	tif->tif_name = (char *)tif + sizeof (TIFF);
	strcpy(tif->tif_name, name);
	tif->tif_fd = fd;
	tif->tif_mode = m &~ (O_CREAT|O_TRUNC);
	tif->tif_curdir = -1;		/* non-existent directory */
	tif->tif_curoff = 0;
	tif->tif_curstrip = -1;		/* invalid strip */
	tif->tif_row = -1;		/* read/write pre-increment */
	
	/*
	 *  Make sure buffer flags are set for both read and write:
	 */
	
	tif->tif_flags |= TIFF_MYBUFFER;
	tif->tif_rawcp = tif->tif_rawdata = 0;
	tif->tif_rawdatasize = 0;
	
	{ int one = 1; bigendian = (*(char *)&one == 0); }
	/*
	 * Read in TIFF header.
	 */
	if (!TIFFReadTIFFHdr(fd, &tif->tif_header)) {
	
		/*
		 *  ******* W R I T E   M O D E ***********
		 */
		
		if (tif->tif_mode == O_RDONLY) {
			TIFFError(name, "Cannot read TIFF header");
			goto bad;
		}
		/*
		 * Setup header and write.
		 */
		tif->tif_header.tiff_magic =  bigendian ?
		    TIFF_BIGENDIAN : TIFF_LITTLEENDIAN;
		tif->tif_header.tiff_version = TIFF_VERSION;
		tif->tif_header.tiff_diroff = 0;	/* filled in later */
		if (!TIFFWriteTIFFHdr(fd, &tif->tif_header)) {
			TIFFError(name, "Error writing TIFF header");
			goto bad;
		}
		/*
		 * Setup the byte order handling.
		 */
		TIFFInitOrder(tif, tif->tif_header.tiff_magic, bigendian);
		/*
		 * Setup default directory.
		 */
		if (!TIFFDefaultDirectory(tif))
			goto bad;
		tif->tif_diroff = 0;
		return (tif);
	}

	/*
	 *  ******* R E A D  /  A P P E N D    M O D E ***********
	 */


	/*
	 * Setup the byte order handling.
	 */
	if (tif->tif_header.tiff_magic != TIFF_BIGENDIAN &&
	    tif->tif_header.tiff_magic != TIFF_LITTLEENDIAN) {
		TIFFError(name,  "Not a TIFF file, bad magic number %d (0x%x)",
		    tif->tif_header.tiff_magic,
		    tif->tif_header.tiff_magic);
		goto bad;
	}
	TIFFInitOrder(tif, tif->tif_header.tiff_magic, bigendian);
	/*
	 * Swap header if required.
	 */
	if (tif->tif_flags & TIFF_SWAB) {
		TIFFSwabShort(&tif->tif_header.tiff_version);
		TIFFSwabLong(&tif->tif_header.tiff_diroff);
	}
	/*
	 * Now check version (if needed, it's been byte-swapped).
	 * Note that this isn't actually a version number, it's a
	 * magic number that doesn't change (stupid).
	 */
	if (tif->tif_header.tiff_version != TIFF_VERSION) {
		TIFFError(name,
		    "Not a TIFF file, bad version number %d (0x%x)",
		    tif->tif_header.tiff_version,
		    tif->tif_header.tiff_version); 
		goto bad;
	}
	/*
	 * Setup initial directory.
	 */
	switch (mode[0]) {
	case 'r':
		tif->tif_nextdiroff = tif->tif_header.tiff_diroff;
#ifdef MMAP_SUPPORT
		if (TIFFMapFileContents(fd, &tif->tif_base, &tif->tif_size))
			tif->tif_flags |= TIFF_MAPPED;
#endif
		if (TIFFReadDirectory(tif)) {
			tif->tif_rawcc = -1;
			tif->tif_flags |= TIFF_BUFFERSETUP;
			return (tif);
		}
		break;
	case 'a':
		/*
		 * Don't append to file that has information
		 * byte swapped -- we will write data that is
		 * in the opposite order.
		 */
		if (tif->tif_flags & TIFF_SWAB) {
			TIFFError(name,
		"Cannot append to file that has opposite byte ordering");
			goto bad;
		}
		/*
		 * New directories are automatically append
		 * to the end of the directory chain when they
		 * are written out (see TIFFWriteDirectory).
		 */
		if (!TIFFDefaultDirectory(tif))
			goto bad;
		return (tif);
	}
bad:
	tif->tif_mode = O_RDONLY;	/* XXX avoid flush */
	TIFFClose(tif);
	return ((TIFF *)0);
bad2:
	(void) close(fd);
	return ((TIFF *)0);
}

tiff_u_long
TIFFScanlineSize(tif)
	TIFF *tif;
{
	TIFFDirectory *td = &tif->tif_dir;
	long scanline;
	
	scanline = td->td_bitspersample * td->td_imagewidth;
	if (td->td_planarconfig == PLANARCONFIG_CONTIG)
		scanline *= td->td_samplesperpixel;
	return ((tiff_u_long)howmany(scanline, 8));
}

static
int TIFFReadTIFFHdr( fd, hdr )
int fd;
TIFFHeader *hdr;
{
	if (!TIFFReadShort( fd, (short *)&hdr->tiff_magic ))
		return (0);
	if (!TIFFReadShort( fd, (short *)&hdr->tiff_version ))
		return (0);
	if (!ReadOK(fd, &hdr->tiff_diroff, TIFFLongSize))
		return (0);
	return (1);
}

int TIFFWriteTIFFHdr( fd, hdr )
int fd;
TIFFHeader *hdr;
{
	if (!TIFFWriteShort( fd, (short *)&hdr->tiff_magic ))
		return (0);
	if (!TIFFWriteShort( fd, (short *)&hdr->tiff_version ))
		return (0);
	if (!WriteOK(fd, &hdr->tiff_diroff, TIFFLongSize))
		return (0);
	return (1);
}

/*
 * Query functions to access private data.
 */

/*
 * Return open file's name.
 */
char *
TIFFFileName(tif)
	TIFF *tif;
{
	return (tif->tif_name);
}

/*
 * Return open file's I/O descriptor.
 */
int
TIFFFileno(tif)
	TIFF *tif;
{
	return (tif->tif_fd);
}

/*
 * Return read/write mode.
 */
int
TIFFGetMode(tif)
	TIFF *tif;
{
	return (tif->tif_mode);
}

/*
 * Return nonzero if file is organized in
 * tiles; zero if organized as strips.
 */
int
TIFFIsTiled(tif)
	TIFF *tif;
{
	return (isTiled(tif));
}

/*
 * Return nonzero if file has 
 * Cartographic Tag Information.
 */
int
TIFFhasCartoTags(tif)
	TIFF *tif;
{
	return (tif->tif_flags & TIFF_HASCARTTAGS);
}

/*
 * Return current row being read/written.
 */
long
TIFFCurrentRow(tif)
	TIFF *tif;
{
	return (tif->tif_row);
}

/*
 * Return index of the current directory.
 */
int
TIFFCurrentDirectory(tif)
	TIFF *tif;
{
	return (tif->tif_curdir);
}

/*
 * Return current strip.
 */
int
TIFFCurrentStrip(tif)
	TIFF *tif;
{
	return (tif->tif_curstrip);
}

/*
 * Return current tile.
 */
int
TIFFCurrentTile(tif)
	TIFF *tif;
{
	return (tif->tif_curtile);
}


#if USE_PROTOTYPES
extern	int TIFFFreeDirectory(TIFF*);
#else
extern	int TIFFFreeDirectory();
#endif

void
TIFFClose(tif)
	TIFF *tif;
{
	if (tif->tif_mode != O_RDONLY)
		/*
		 * Flush buffered data and directory (if dirty).
		 */
		TIFFFlush(tif);
	if (tif->tif_cleanup)
		(*tif->tif_cleanup)(tif);
	TIFFFreeDirectory(tif);
	if (tif->tif_rawdata && (tif->tif_flags&TIFF_MYBUFFER))
		free(tif->tif_rawdata);
#ifdef MMAP_SUPPORT
	if (isMapped(tif))
		TIFFUnmapFileContents(tif->tif_base, tif->tif_size);
#endif
	(void) close(tif->tif_fd);
	free((char *)tif);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_lzw.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_lzw.c,v 1.37 92/02/12 11:27:28 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 * Rev 5.0 Lempel-Ziv & Welch Compression Support
 *
 * This code is derived from the compress program whose code is
 * derived from software contributed to Berkeley by James A. Woods,
 * derived from original work by Spencer Thomas and Joseph Orost.
 *
 * The original Berkeley copyright notice appears below in its entirety.
 */
#include "tiffiop.h"
#include <stdio.h>
#include <assert.h>
#include "prototypes.h"
#include <strings.h>

#define MAXCODE(n)	((1 << (n)) - 1)
/*
 * The TIFF spec specifies that encoded bit strings range
 * from 9 to 12 bits.  This is somewhat unfortunate in that
 * experience indicates full color RGB pictures often need
 * ~14 bits for reasonable compression.
 */
#define	BITS_MIN	9		/* start with 9 bits */
#define	BITS_MAX	12		/* max of 12 bit strings */
/* predefined codes */
#define	CODE_CLEAR	256		/* code to clear string table */
#define	CODE_EOI	257		/* end-of-information code */
#define CODE_FIRST	258		/* first free code entry */
#define	CODE_MAX	MAXCODE(BITS_MAX)
#ifdef notdef
#define	HSIZE		9001		/* 91% occupancy */
#define	HSHIFT		(8-(16-13))
#else
#define	HSIZE		5003		/* 80% occupancy */
#define	HSHIFT		(8-(16-12))
#endif

/*
 * NB: The 5.0 spec describes a different algorithm than Aldus
 *     implements.  Specifically, Aldus does code length transitions
 *     one code earlier than should be done (for real LZW).
 *     Earlier versions of this library implemented the correct
 *     LZW algorithm, but emitted codes in a bit order opposite
 *     to the TIFF spec.  Thus, to maintain compatibility w/ Aldus
 *     we interpret MSB-LSB ordered codes to be images written w/
 *     old versions of this library, but otherwise adhere to the
 *     Aldus "off by one" algorithm.
 *
 * Future revisions to the TIFF spec are expected to "clarify this issue".
 */
#define	SetMaxCode(sp, v) {			\
	(sp)->lzw_maxcode = (v)-1;		\
	if ((sp)->lzw_flags & LZW_COMPAT)	\
		(sp)->lzw_maxcode++;		\
}

/*
 * Decoding-specific state.
 */
struct decode {
	short	prefixtab[HSIZE];	/* prefix(code) */
	u_char	suffixtab[CODE_MAX+1];	/* suffix(code) */
	u_char	stack[HSIZE-(CODE_MAX+1)];
	u_char	*stackp;		/* stack pointer */
	int	firstchar;		/* of string associated w/ last code */
};

/*
 * Encoding-specific state.
 */
struct encode {
	long	checkpoint;		/* point at which to clear table */
#define CHECK_GAP	10000		/* enc_ratio check interval */
	long	ratio;			/* current compression ratio */
	long	incount;		/* (input) data bytes encoded */
	long	outcount;		/* encoded (output) bytes */
	long	htab[HSIZE];		/* hash table */
	short	codetab[HSIZE];		/* code table */
};

#if USE_PROTOTYPES
typedef	void (*predictorFunc)(char* data, long nbytes, int stride);
extern int TIFFInitLZW(register TIFF *);
#else
typedef	void (*predictorFunc)();
extern int TIFFInitLZW();
#endif

/*
 * State block for each open TIFF
 * file using LZW compression/decompression.
 */
typedef	struct {
	int	lzw_oldcode;		/* last code encountered */
	u_short	lzw_flags;
#define	LZW_RESTART	0x01		/* restart interrupted decode */
#define	LZW_COMPAT	0x02		/* read old bit-reversed codes */
	u_short	lzw_nbits;		/* number of bits/code */
	u_short	lzw_stride;		/* horizontal diferencing stride */
	tiff_u_long	lzw_rowsize;		/* XXX maybe should be a long? YES. */
	predictorFunc lzw_hordiff;
	int	lzw_maxcode;		/* maximum code for lzw_nbits */
	long	lzw_bitoff;		/* bit offset into data */
	long	lzw_bitsize;		/* size of strip in bits */
	int	lzw_free_ent;		/* next free entry in hash table */
	union {
		struct	decode dec;
		struct	encode enc;
	} u;
} LZWState;
#define	dec_prefix	u.dec.prefixtab
#define	dec_suffix	u.dec.suffixtab
#define	dec_stack	u.dec.stack
#define	dec_stackp	u.dec.stackp
#define	dec_firstchar	u.dec.firstchar

#define	enc_checkpoint	u.enc.checkpoint
#define	enc_ratio	u.enc.ratio
#define	enc_incount	u.enc.incount
#define	enc_outcount	u.enc.outcount
#define	enc_htab	u.enc.htab
#define	enc_codetab	u.enc.codetab

/* masks for extracting/inserting variable length bit codes */
static const u_char rmask[9] =
    { 0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff };
static const u_char lmask[9] =
    { 0x00, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff };

#if USE_PROTOTYPES
static	int LZWPreEncode(TIFF*);
static	int LZWEncode(TIFF*, u_char*, tiff_u_long, u_int);
static	int LZWEncodePredRow(TIFF*, u_char*, tiff_u_long, u_int);
static	int LZWEncodePredTile(TIFF*, u_char*, tiff_u_long, u_int);
static	int LZWPostEncode(TIFF*);
static	int LZWDecode(TIFF*, u_char*, tiff_u_long, u_int);
static	int LZWDecodePredRow(TIFF*, u_char*, tiff_u_long, u_int);
static	int LZWDecodePredTile(TIFF*, u_char*, tiff_u_long, u_int);
static	int LZWPreDecode(TIFF*);
static	int LZWCleanup(TIFF*);
static	int GetNextCode(TIFF*);
static	void PutNextCode(TIFF*, int);
static	void cl_block(TIFF*);
static	void cl_hash(LZWState*);
extern	int TIFFFlushData1(TIFF *);
#else
static	int LZWPreEncode(), LZWEncode(), LZWPostEncode();
static	int LZWEncodePredRow(), LZWEncodePredTile();
static	int LZWPreDecode(), LZWDecode();
static	int LZWDecodePredRow(), LZWDecodePredTile();
static	int LZWCleanup();
static	int GetNextCode();
static	void PutNextCode();
static	void cl_block();
static	void cl_hash();
extern	int TIFFFlushData1();
#endif

TIFFInitLZW(tif)
	TIFF *tif;
{
	tif->tif_predecode = LZWPreDecode;
	tif->tif_decoderow = LZWDecode;
	tif->tif_decodestrip = LZWDecode;
	tif->tif_decodetile = LZWDecode;
	tif->tif_preencode = LZWPreEncode;
	tif->tif_postencode = LZWPostEncode;
	tif->tif_encoderow = LZWEncode;
	tif->tif_encodestrip = LZWEncode;
	tif->tif_encodetile = LZWEncode;
	tif->tif_cleanup = LZWCleanup;
	return (1);
}

static
DECLARE4(LZWCheckPredictor,
	TIFF*, tif,
	LZWState*, sp,
	predictorFunc, pred8bit,
	predictorFunc, pred16bit
)
{
	TIFFDirectory *td = &tif->tif_dir;

	switch (td->td_predictor) {
	case 1:
		break;
	case 2:
		sp->lzw_stride = (td->td_planarconfig == PLANARCONFIG_CONTIG ?
		    td->td_samplesperpixel : 1);
		switch (td->td_bitspersample) {
		case 8:
			sp->lzw_hordiff = pred8bit;
			break;
		case 16:
			sp->lzw_hordiff = pred16bit;
			break;
		default:
			TIFFError(tif->tif_name,
    "Horizontal differencing \"Predictor\" not supported with %d-bit samples",
			    td->td_bitspersample);
			return (0);
		}
		break;
	default:
		TIFFError(tif->tif_name, "\"Predictor\" value %d not supported",
		    td->td_predictor);
		return (0);
	}
	if (sp->lzw_hordiff) {
		/*
		 * Calculate the scanline/tile-width size in bytes.
		 */
		if (isTiled(tif))
			sp->lzw_rowsize = TIFFTileRowSize(tif);
		else
			sp->lzw_rowsize = TIFFScanlineSize(tif);
	}
	return (1);
}

/*
 * LZW Decoder.
 */

#define REPEAT4(n, op)		\
    switch (n) {		\
    default: { int i; for (i = n-4; i > 0; i--) { op; } } \
    case 4:  op;		\
    case 3:  op;		\
    case 2:  op;		\
    case 1:  op;		\
    case 0:  ;			\
    }

static void
DECLARE3(horizontalAccumulate8,
	register char*, cp,
	register long, cc,
	register int, stride
)
{
	if (cc > stride) {
		cc -= stride;
		do {
			REPEAT4(stride, cp[stride] += cp[0]; cp++)
			cc -= stride;
		} while (cc > 0);
	}
}

static void
DECLARE3(horizontalAccumulate16,
	char*, cp,
	long, cc,
	register int, stride
)
{
	register short* wp = (short *)cp;
	register int wc = cc / 2;

	if (wc > stride) {
		wc -= stride;
		do {
			REPEAT4(stride, wp[stride] += wp[0]; wp++)
			wc -= stride;
		} while (wc > 0);
	}
}

/*
 * Setup state for decoding a strip.
 */
static
LZWPreDecode(tif)
	TIFF *tif;
{
	register LZWState *sp = (LZWState *)tif->tif_data;
	register int code;

	if (sp == NULL) {
		tif->tif_data = malloc(sizeof (LZWState));
		if (tif->tif_data == NULL) {
			TIFFError("LZWPreDecode",
			    "No space for LZW state block");
			return (0);
		}
		sp = (LZWState *)tif->tif_data;
		sp->lzw_flags = 0;
		sp->lzw_hordiff = 0;
		sp->lzw_rowsize = 0;
		if (!LZWCheckPredictor(tif, sp,
		  horizontalAccumulate8, horizontalAccumulate16))
			return (0);
		if (sp->lzw_hordiff) {
			/*
			 * Override default decoding method with
			 * one that does the predictor stuff.
			 */
			tif->tif_decoderow = LZWDecodePredRow;
			tif->tif_decodestrip = LZWDecodePredTile;
			tif->tif_decodetile = LZWDecodePredTile;
		}
	} else
		sp->lzw_flags &= ~LZW_RESTART;
	sp->lzw_nbits = BITS_MIN;
	/*
	 * Pre-load the table.
	 */
	for (code = 255; code >= 0; code--)
		sp->dec_suffix[code] = (u_char)code;
	sp->lzw_free_ent = CODE_FIRST;
	sp->lzw_bitoff = 0;
	/* calculate data size in bits */
	sp->lzw_bitsize = tif->tif_rawdatasize;
	sp->lzw_bitsize = (sp->lzw_bitsize << 3) - (BITS_MAX-1);
	sp->dec_stackp = sp->dec_stack;
	sp->lzw_oldcode = -1;
	sp->dec_firstchar = -1;
	/*
	 * Check for old bit-reversed codes.  All the flag
	 * manipulations are to insure only one warning is
	 * given for a file.
	 */
	if (tif->tif_rawdata[0] == 0 && (tif->tif_rawdata[1] & 0x1)) {
		if ((sp->lzw_flags & LZW_COMPAT) == 0)
			TIFFWarning(tif->tif_name,
			    "Old-style LZW codes, convert file");
		sp->lzw_flags |= LZW_COMPAT;
	} else
		sp->lzw_flags &= ~LZW_COMPAT;
	SetMaxCode(sp, MAXCODE(BITS_MIN));
	return (1);
}

/*
 * Decode a "hunk of data".
 */
static
LZWDecode(tif, op0, occ0, s)
	TIFF *tif;
	u_char *op0;
	tiff_u_long occ0;
	u_int s;
{
	register char *op = (char *)op0;
	register long occ = occ0;
	register LZWState *sp = (LZWState *)tif->tif_data;
	register int code;
	register u_char *stackp;
	int firstchar, oldcode, incode;

	stackp = sp->dec_stackp;
	/*
	 * Restart interrupted unstacking operations.
	 */
	if (sp->lzw_flags & LZW_RESTART) {
		do {
			if (--occ < 0) {	/* end of scanline */
				sp->dec_stackp = stackp;
				return (1);
			}
			*op++ = *--stackp;
		} while (stackp > sp->dec_stack);
		sp->lzw_flags &= ~LZW_RESTART;
	}
	oldcode = sp->lzw_oldcode;
	firstchar = sp->dec_firstchar;
	while (occ > 0 && (code = GetNextCode(tif)) != CODE_EOI) {
		if (code == CODE_CLEAR) {
			mybzero(sp->dec_prefix, sizeof (sp->dec_prefix));
			sp->lzw_free_ent = CODE_FIRST;
			sp->lzw_nbits = BITS_MIN;
			SetMaxCode(sp, MAXCODE(BITS_MIN));
			if ((code = GetNextCode(tif)) == CODE_EOI)
				break;
			*op++ = code, occ--;
			oldcode = firstchar = code;
			continue;
		}
		incode = code;
		/*
		 * When a code is not in the table we use (as spec'd):
		 *    StringFromCode(oldcode) +
		 *        FirstChar(StringFromCode(oldcode))
		 */
		if (code >= sp->lzw_free_ent) {	/* code not in table */
			*stackp++ = firstchar;
			code = oldcode;
		}

		/*
		 * Generate output string (first in reverse).
		 */
		for (; code >= 256; code = sp->dec_prefix[code])
			*stackp++ = sp->dec_suffix[code];
		*stackp++ = firstchar = sp->dec_suffix[code];
		do {
			if (--occ < 0) {	/* end of scanline */
				sp->lzw_flags |= LZW_RESTART;
				break;
			}
			*op++ = *--stackp;
		} while (stackp > sp->dec_stack);

		/*
		 * Add the new entry to the code table.
		 */
		if ((code = sp->lzw_free_ent) < CODE_MAX) {
			sp->dec_prefix[code] = (u_short)oldcode;
			sp->dec_suffix[code] = firstchar;
			sp->lzw_free_ent++;
			/*
			 * If the next entry is too big for the
			 * current code size, then increase the
			 * size up to the maximum possible.
			 */
			if (sp->lzw_free_ent > sp->lzw_maxcode) {
				sp->lzw_nbits++;
				if (sp->lzw_nbits > BITS_MAX)
					sp->lzw_nbits = BITS_MAX;
				SetMaxCode(sp, MAXCODE(sp->lzw_nbits));
			}
		} 
		oldcode = incode;
	}
	sp->dec_stackp = stackp;
	sp->lzw_oldcode = oldcode;
	sp->dec_firstchar = firstchar;
	if (occ > 0) {
		TIFFError(tif->tif_name,
		"LZWDecode: Not enough data at scanline %d (short %ld bytes)",
		    tif->tif_row, occ);
		return (0);
	}
	return (1);
}

/*
 * Decode a scanline and apply the predictor routine.
 */
static
LZWDecodePredRow(tif, op0, occ0, s)
	TIFF *tif;
	u_char *op0;
	tiff_u_long occ0;
	u_int s;
{
	LZWState *sp = (LZWState *)tif->tif_data;

	if (LZWDecode(tif, op0, occ0, s)) {
		(*sp->lzw_hordiff)((char *)op0, occ0, sp->lzw_stride);
		return (1);
	} else
		return (0);
}

/*
 * Decode a tile/strip and apply the predictor routine.
 * Note that horizontal differencing must be done on a
 * row-by-row basis.  The width of a "row" has already
 * been calculated at pre-decode time according to the
 * strip/tile dimensions.
 */
static
LZWDecodePredTile(tif, op0, occ0, s)
	TIFF *tif;
	u_char *op0;
	tiff_u_long occ0;
	u_int s;
{
	LZWState *sp = (LZWState *)tif->tif_data;

	if (!LZWDecode(tif, op0, occ0, s))
		return (0);
	while (occ0 > 0) {
		(*sp->lzw_hordiff)((char *)op0, sp->lzw_rowsize, sp->lzw_stride);
		occ0 -= sp->lzw_rowsize;
		op0 += sp->lzw_rowsize;
	}
	return (1);
}

/*
 * Get the next code from the raw data buffer.
 */
static
GetNextCode(tif)
	TIFF *tif;
{
	register LZWState *sp = (LZWState *)tif->tif_data;
	register int code, bits;
	register long r_off;
	register u_char *bp;

	/*
	 * This check shouldn't be necessary because each
	 * strip is suppose to be terminated with CODE_EOI.
	 * At worst it's a substitute for the CODE_EOI that's
	 * supposed to be there (see calculation of lzw_bitsize
	 * in LZWPreDecode()).
	 */
	if (sp->lzw_bitoff > sp->lzw_bitsize) {
		TIFFWarning(tif->tif_name,
		    "LZWDecode: Strip %d not terminated with EOI code",
		    tif->tif_curstrip);
		return (CODE_EOI);
	}
	r_off = sp->lzw_bitoff;
	bits = sp->lzw_nbits;
	/*
	 * Get to the first byte.
	 */
	bp = (u_char *)tif->tif_rawdata + (r_off >> 3);
	r_off &= 7;
	if (sp->lzw_flags & LZW_COMPAT) {
		/* Get first part (low order bits) */
		code = (*bp++ >> r_off);
		r_off = 8 - r_off;		/* now, offset into code word */
		bits -= r_off;
		/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
		if (bits >= 8) {
			code |= *bp++ << r_off;
			r_off += 8;
			bits -= 8;
		}
		/* high order bits. */
		code |= (*bp & rmask[bits]) << r_off;
	} else {
		r_off = 8 - r_off;		/* convert offset to count */
		code = *bp++ & rmask[r_off];	/* high order bits */
		bits -= r_off;
		if (bits >= 8) {
			code = (code<<8) | *bp++;
			bits -= 8;
		}
		/* low order bits */
		code = (code << bits) |
		    (((unsigned)(*bp & lmask[bits])) >> (8 - bits));
	}
	sp->lzw_bitoff += sp->lzw_nbits;
	return (code);
}

/*
 * LZW Encoding.
 */

static void
DECLARE3(horizontalDifference8,
	register char*, cp,
	register long, cc,
	register int, stride
)
{
	if (cc > stride) {
		cc -= stride;
		cp += cc - 1;
		do {
			REPEAT4(stride, cp[stride] -= cp[0]; cp--)
			cc -= stride;
		} while (cc > 0);
	}
}

static void
DECLARE3(horizontalDifference16,
	char*, cp,
	long, cc,
	register int, stride
)
{
	register short *wp = (short *)cp;
	register long wc = cc/2;

	if (wc > stride) {
		wc -= stride;
		wp += wc - 1;
		do {
			REPEAT4(stride, wp[stride] -= wp[0]; wp--)
			wc -= stride;
		} while (wc > 0);
	}
}

/*
 * Reset encoding state at the start of a strip.
 */
static
LZWPreEncode(tif)
	TIFF *tif;
{
	register LZWState *sp = (LZWState *)tif->tif_data;

	if (sp == NULL) {
		tif->tif_data = malloc(sizeof (LZWState));
		if (tif->tif_data == NULL) {
			TIFFError("LZWPreEncode",
			    "No space for LZW state block");
			return (0);
		}
		sp = (LZWState *)tif->tif_data;
		sp->lzw_flags = 0;
		sp->lzw_hordiff = 0;
		if (!LZWCheckPredictor(tif, sp,
		    horizontalDifference8, horizontalDifference16))
			return (0);
		if (sp->lzw_hordiff) {
			tif->tif_encoderow = LZWEncodePredRow;
			tif->tif_encodestrip = LZWEncodePredTile;
			tif->tif_encodetile = LZWEncodePredTile;
		}
	}
	sp->enc_checkpoint = CHECK_GAP;
	SetMaxCode(sp, MAXCODE(sp->lzw_nbits = BITS_MIN)+1);
	cl_hash(sp);		/* clear hash table */
	sp->lzw_bitoff = 0;
	sp->lzw_bitsize = (tif->tif_rawdatasize << 3) - (BITS_MAX-1);
	sp->lzw_oldcode = -1;	/* generates CODE_CLEAR in LZWEncode */
	return (1);
}

/*
 * Encode a scanline of pixels.
 *
 * Uses an open addressing double hashing (no chaining) on the 
 * prefix code/next character combination.  We do a variant of
 * Knuth's algorithm D (vol. 3, sec. 6.4) along with G. Knott's
 * relatively-prime secondary probe.  Here, the modular division
 * first probe is gives way to a faster exclusive-or manipulation. 
 * Also do block compression with an adaptive reset, whereby the
 * code table is cleared when the compression ratio decreases,
 * but after the table fills.  The variable-length output codes
 * are re-sized at this point, and a CODE_CLEAR is generated
 * for the decoder. 
 */
static
LZWEncode(tif, bp, cc, s)
	TIFF *tif;
	u_char *bp;
	tiff_u_long cc;
	u_int s;
{
	static char module[] = "LZWEncode";
	register LZWState *sp;
	register long fcode;
	register long h, c, ent, disp;

	if ((sp = (LZWState *)tif->tif_data) == NULL)
		return (0);
	ent = sp->lzw_oldcode;
	if (ent == -1 && cc > 0) {
		PutNextCode(tif, CODE_CLEAR);
		ent = *bp++; cc--; sp->enc_incount++;
	}
	while (cc > 0) {
		c = *bp++; cc--; sp->enc_incount++;
		fcode = ((long)c << BITS_MAX) + ent;
		h = (c << HSHIFT) ^ ent;	/* xor hashing */
		if (sp->enc_htab[h] == fcode) {
			ent = sp->enc_codetab[h];
			continue;
		}
		if (sp->enc_htab[h] >= 0) {
			/*
			 * Primary hash failed, check secondary hash.
			 */
			disp = HSIZE - h;
			if (h == 0)
				disp = 1;
			do {
				if ((h -= disp) < 0)
					h += HSIZE;
				if (sp->enc_htab[h] == fcode) {
					ent = sp->enc_codetab[h];
					goto hit;
				}
			} while (sp->enc_htab[h] >= 0);
		}
		/*
		 * New entry, emit code and add to table.
		 */
		PutNextCode(tif, ent);
		ent = c;
		sp->enc_codetab[h] = sp->lzw_free_ent++;
		sp->enc_htab[h] = fcode;
		if (sp->lzw_free_ent == CODE_MAX-1) {
			/* table is full, emit clear code and reset */
			cl_hash(sp);
			PutNextCode(tif, CODE_CLEAR);
			SetMaxCode(sp, MAXCODE(sp->lzw_nbits = BITS_MIN)+1);
		} else {
			/*
			 * If the next entry is going to be too big for
			 * the code size, then increase it, if possible.
			 */
			if (sp->lzw_free_ent > sp->lzw_maxcode) {
				sp->lzw_nbits++;
				assert(sp->lzw_nbits <= BITS_MAX);
				SetMaxCode(sp, MAXCODE(sp->lzw_nbits)+1);
			} else if (sp->enc_incount >= sp->enc_checkpoint)
				cl_block(tif);
		}
	hit:
		;
	}
	sp->lzw_oldcode = ent;
	return (1);
}

static
LZWEncodePredRow(tif, bp, cc, s)
	TIFF *tif;
	u_char *bp;
	tiff_u_long cc;
	u_int s;
{
	LZWState *sp = (LZWState *)tif->tif_data;

/* XXX horizontal differencing alters user's data XXX */
	(*sp->lzw_hordiff)((char *)bp, cc, sp->lzw_stride);
	return (LZWEncode(tif, bp, cc, s));
}

static
LZWEncodePredTile(tif, bp0, cc0, s)
	TIFF *tif;
	u_char *bp0;
	tiff_u_long cc0;
	u_int s;
{
	LZWState *sp = (LZWState *)tif->tif_data;
	tiff_u_long cc = cc0;
	u_char *bp = bp0;

	while (cc > 0) {
		(*sp->lzw_hordiff)((char *)bp, sp->lzw_rowsize, sp->lzw_stride);
		cc -= sp->lzw_rowsize;
		bp += sp->lzw_rowsize;
	}
	return (LZWEncode(tif, bp0, cc0, s));
}

/*
 * Finish off an encoded strip by flushing the last
 * string and tacking on an End Of Information code.
 */
static
LZWPostEncode(tif)
	TIFF *tif;
{
	LZWState *sp = (LZWState *)tif->tif_data;

	if (sp->lzw_oldcode != -1) {
		PutNextCode(tif, sp->lzw_oldcode);
		sp->lzw_oldcode = -1;
	}
	PutNextCode(tif, CODE_EOI);
	return (1);
}

static void
PutNextCode(tif, c)
	TIFF *tif;
	int c;
{
	register LZWState *sp = (LZWState *)tif->tif_data;
	register long r_off;
	register int bits, code = c;
	register char *bp;

	r_off = sp->lzw_bitoff;
	bits = sp->lzw_nbits;
	/*
	 * Flush buffer if code doesn't fit.
	 */
	if (r_off + bits > sp->lzw_bitsize) {
		/*
		 * Calculate the number of full bytes that can be
		 * written and save anything else for the next write.
		 */
		if (r_off & 7) {
			tif->tif_rawcc = r_off >> 3;
			bp = tif->tif_rawdata + tif->tif_rawcc;
			(void) TIFFFlushData1(tif);
			tif->tif_rawdata[0] = *bp;
		} else {
			/*
			 * Otherwise, on a byte boundary (in
			 * which tiff_rawcc is already correct).
			 */
			(void) TIFFFlushData1(tif);
		}
		bp = tif->tif_rawdata;
		sp->lzw_bitoff = (r_off &= 7);
	} else {
		/*
		 * Get to the first byte.
		 */
		bp = tif->tif_rawdata + (r_off >> 3);
		r_off &= 7;
	}
	/*
	 * Note that lzw_bitoff is maintained as the bit offset
	 * into the buffer w/ a right-to-left orientation (i.e.
	 * lsb-to-msb).  The bits, however, go in the file in
	 * an msb-to-lsb order.
	 */
	bits -= (8 - r_off);
	*bp = (*bp & lmask[r_off]) | (code >> bits);
	bp++;
	if (bits >= 8) {
		bits -= 8;
		*bp++ = code >> bits;
	}
	if (bits)
		*bp = (code & rmask[bits]) << (8 - bits);
	/*
	 * enc_outcount is used by the compression analysis machinery
	 * which resets the compression tables when the compression
	 * ratio goes up.  lzw_bitoff is used here (in PutNextCode) for
	 * inserting codes into the output buffer.  tif_rawcc must
	 * be updated for the mainline write code in TIFFWriteScanline()
	 * so that data is flushed when the end of a strip is reached.
	 * Note that the latter is rounded up to ensure that a non-zero
	 * byte count is present. 
	 */
	sp->enc_outcount += sp->lzw_nbits;
	sp->lzw_bitoff += sp->lzw_nbits;
	tif->tif_rawcc = (sp->lzw_bitoff + 7) >> 3;
}

/*
 * Check compression ratio and, if things seem to
 * be slipping, clear the hash table and reset state.
 */
static void
cl_block(tif)
	TIFF *tif;
{
	register LZWState *sp = (LZWState *)tif->tif_data;
	register long rat;

	sp->enc_checkpoint = sp->enc_incount + CHECK_GAP;
	if (sp->enc_incount > 0x007fffff) {	/* shift will overflow */
		rat = sp->enc_outcount >> 8;
		rat = (rat == 0 ? 0x7fffffff : sp->enc_incount / rat);
	} else
		rat = (sp->enc_incount<<8)/sp->enc_outcount; /* 8 fract bits */
	if (rat <= sp->enc_ratio) {
		cl_hash(sp);
		PutNextCode(tif, CODE_CLEAR);
		SetMaxCode(sp, MAXCODE(sp->lzw_nbits = BITS_MIN)+1);
	} else
		sp->enc_ratio = rat;
}

/*
 * Reset code table and related statistics.
 */
static void
cl_hash(sp)
	LZWState *sp;
{
	register long *htab_p = sp->enc_htab+HSIZE;
	register long i, m1 = -1;

	i = HSIZE - 16;
 	do {
		*(htab_p-16) = m1;
		*(htab_p-15) = m1;
		*(htab_p-14) = m1;
		*(htab_p-13) = m1;
		*(htab_p-12) = m1;
		*(htab_p-11) = m1;
		*(htab_p-10) = m1;
		*(htab_p-9) = m1;
		*(htab_p-8) = m1;
		*(htab_p-7) = m1;
		*(htab_p-6) = m1;
		*(htab_p-5) = m1;
		*(htab_p-4) = m1;
		*(htab_p-3) = m1;
		*(htab_p-2) = m1;
		*(htab_p-1) = m1;
		htab_p -= 16;
	} while ((i -= 16) >= 0);
    	for (i += 16; i > 0; i--)
		*--htab_p = m1;

	sp->enc_ratio = 0;
	sp->enc_incount = 0;
	sp->enc_outcount = 0;
	sp->lzw_free_ent = CODE_FIRST;
}

static
LZWCleanup(tif)
	TIFF *tif;
{
	if (tif->tif_data) {
		free(tif->tif_data);
		tif->tif_data = NULL;
	}
}

/*
 * Copyright (c) 1985, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James A. Woods, derived from original work by Spencer Thomas
 * and Joseph Orost.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_mach.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_machdep.c,v 1.2 92/03/06 11:52:42 sam Exp $";
#endif

/*
 * Copyright (c) 1992 Sam Leffler
 * Copyright (c) 1992 Silicon Graphics, Inc.
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

/*
 * TIFF Library Machine Dependent Routines.
 */
#include "tiffiop.h"

#ifdef tahoe
typedef	struct ieeedouble {
	tiff_u_long	sign	: 1,
		exp	: 11,
		mant	: 20;
	tiff_u_long	mant2;
} ieeedouble;
typedef	struct ieeefloat {
	tiff_u_long	sign	: 1,
		exp	: 8,
		mant	: 23;
} ieeefloat;

typedef	struct {
	tiff_u_long	sign	: 1,
		exp	: 8,
		mant	: 23;
	tiff_u_long	mant2;
} nativedouble;
typedef	struct {
	tiff_u_long	sign	: 1,
		exp	: 8,
		mant	: 23;
} nativefloat;
/*
 * Beware, over/under-flow in conversions will
 * result in garbage values -- handling it would
 * require a subroutine call or lots more code.
 */
#define	NATIVE2IEEEFLOAT(fp) { \
    if ((fp)->native.exp) \
        (fp)->ieee.exp = (fp)->native.exp - 129 + 127;	/* alter bias */\
}
#define	IEEEFLOAT2NATIVE(fp) { \
    if ((fp)->ieee.exp) \
        (fp)->native.exp = (fp)->ieee.exp - 127 + 129; 	/* alter bias */\
}
#define	IEEEDOUBLE2NATIVE(dp) { \
    if ((dp)->native.exp = (dp)->ieee.exp) \
	(dp)->native.exp += -1023 + 129; \
    (dp)->native.mant = ((dp)->ieee.mant<<3)|((dp)->native.mant2>>29); \
    (dp)->native.mant2 <<= 3; \
}
#endif /* tahoe */

#ifdef vms
typedef	struct ieeedouble {
	tiff_u_long	mant	: 20,
		exp	: 11,
		sign	: 1;
	tiff_u_long	mant2;
} ieeedouble;
typedef	struct ieeefloat {
	tiff_u_long	mant	: 23,
		exp	: 8,
		sign	: 1;
} ieeefloat;

typedef	struct {
	tiff_u_long	mant1	: 7,
		exp	: 8,
		sign	: 1,
		mant2	: 16;
	tiff_u_long	mant3;
} nativedouble;
typedef	struct {
	tiff_u_long	mant1	: 7,
		exp	: 8,
		sign	: 1,
		mant2	: 16;
} nativefloat;
/*
 * Beware, these do not handle over/under-flow
 * during conversion from ieee to native format.
 */
#define	NATIVE2IEEEFLOAT(fp) { \
    float_t t; \
    if (t.ieee.exp = (fp)->native.exp) \
	t.ieee.exp += -129 + 127; \
    t.ieee.sign = (fp)->native.sign; \
    t.ieee.mant = ((fp)->native.mant1<<16)|(fp)->native.mant2; \
    *(fp) = t; \
}
#define	IEEEFLOAT2NATIVE(fp) { \
    float_t t; int v = (fp)->ieee.exp; \
    if (v) v += -127 + 129;		/* alter bias of exponent */\
    t.native.exp = v;			/* implicit truncation of exponent */\
    t.native.sign = (fp)->ieee.sign; \
    v = (fp)->ieee.mant; \
    t.native.mant1 = v >> 16; \
    t.native.mant2 = v;\
    *(fp) = t; \
}
#define	IEEEDOUBLE2NATIVE(dp) { \
    double_t t; int v = (dp)->ieee.exp; \
    if (v) v += -1023 + 129; 		/* if can alter bias of exponent */\
    t.native.exp = v;			/* implicit truncation of exponent */\
    v = (dp)->ieee.mant; \
    t.native.sign = (dp)->ieee.sign; \
    t.native.mant1 = v >> 16; \
    t.native.mant2 = v;\
    t.native.mant3 = (dp)->mant2; \
    *(dp) = t; \
}
#endif /* vms */

#if !HAVE_IEEEFP
#if !defined(IEEEFLOAT2NATIVE) || !defined(NATIVE2IEEEFLOAT)
"Help, you've configured the library to not have IEEE floating point,\
but not defined how to convert between IEEE and native formats!"
#endif

/*
 * These unions are used during floating point
 * conversions.  The above macros define the
 * conversion operations.
 */
typedef	union {
	ieeedouble	ieee;
	nativedouble	native;
	char		b[8];
	double		d;
} double_t;

typedef	union {
	ieeefloat	ieee;
	nativefloat	native;
	char		b[4];
	float		f;
} float_t;

TIFFCvtIEEEFloatToNative(tif, n, f)
	TIFF *tif;
	u_int n;
	float *f;
{
	float_t *fp = (float_t *)f;

	while (n-- > 0) {
		IEEEFLOAT2NATIVE(fp);
		fp++;
	}
}

TIFFCvtNativeToIEEEFloat(tif, n, f)
	TIFF *tif;
	u_int n;
	float *f;
{
	float_t *fp = (float_t *)f;

	while (n-- > 0) {
		NATIVE2IEEEFLOAT(fp);
		fp++;
	}
}
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_pack.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_packbits.c,v 1.23 92/03/30 18:29:40 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * PackBits Compression Algorithm Support
 */
#include "tiffiop.h"
#include <stdio.h>
#include <assert.h>
#include <strings.h>
#include <stdint.h>	/* for 'uintptr_t' */

#if USE_PROTOTYPES
static	int PackBitsPreEncode(TIFF *);
static	int PackBitsEncode(TIFF *, u_char *, tiff_u_long, u_int);
static	int PackBitsEncodeChunk(TIFF *, u_char *, tiff_u_long, u_int);
static	int PackBitsDecode(TIFF *, u_char *, tiff_u_long, u_int);
extern	int TIFFInitPackBits(TIFF*);
#else
static	int PackBitsPreEncode();
static	int PackBitsEncode(), PackBitsEncodeChunk();
static	int PackBitsDecode();
extern	int TIFFInitPackBits();
#endif

TIFFInitPackBits(tif)
	TIFF *tif;
{
	tif->tif_decoderow = PackBitsDecode;
	tif->tif_decodestrip = PackBitsDecode;
	tif->tif_decodetile = PackBitsDecode;
	tif->tif_preencode = PackBitsPreEncode;
	tif->tif_encoderow = PackBitsEncode;
	tif->tif_encodestrip = PackBitsEncodeChunk;
	tif->tif_encodetile = PackBitsEncodeChunk;
	return (1);
}

static int
PackBitsPreEncode(tif)
	TIFF *tif;
{
	/*
	 * Calculate the scanline/tile-width size in bytes.
	 */
	if (isTiled(tif))
		/*tif->tif_data = (char *) TIFFTileRowSize(tif);
		  above gives compile warnings on 64-bit Linux, replace with: */
		tif->tif_data = (char *)(uintptr_t) TIFFTileRowSize(tif);
	else
		/*tif->tif_data = (char *) TIFFScanlineSize(tif); as above: */
		tif->tif_data = (char *)(uintptr_t) TIFFScanlineSize(tif);
	return (1);
}

/*
 * Encode a rectangular chunk of pixels.  We break it up
 * into row-sized pieces to insure that encoded runs do
 * not span rows.  Otherwise, there can be problems with
 * the decoder if data is read, for example, by scanlines
 * when it was encoded by strips.
 */
static int
PackBitsEncodeChunk(tif, bp, cc, s)
	TIFF *tif;
	u_char *bp;
	tiff_u_long cc;
	u_int s;
{
	tiff_u_long rowsize = (tiff_u_long)(uintptr_t) tif->tif_data;

	assert(rowsize > 0);
	while (cc > 0) {
		if (PackBitsEncode(tif, bp, rowsize, s) < 0)
			return (-1);
		bp += rowsize;
		cc -= rowsize;
	}
	return (1);
}

/*
 * Encode a run of pixels.
 */
static int
PackBitsEncode(tif, bp, cc, s)
	TIFF *tif;
	u_char *bp;
	register tiff_u_long cc;
	u_int s;
{
	register char *op, *lastliteral;
	register int n, b;
	enum { BASE, LITERAL, RUN, LITERAL_RUN } state;
	char *ep;
	int slop;

	op = tif->tif_rawcp;
	ep = tif->tif_rawdata + tif->tif_rawdatasize;
	state = BASE;
	lastliteral = 0;
	while (cc > 0) {
		/*
		 * Find the longest string of identical bytes.
		 */
		b = *bp++, cc--, n = 1;
		for (; cc > 0 && b == *bp; cc--, bp++)
			n++;
	again:
		if (op + 2 >= ep) {		/* insure space for new data */
			/*
			 * Be careful about writing the last
			 * literal.  Must write up to that point
			 * and then copy the remainder to the
			 * front of the buffer.
			 */
			if (state == LITERAL || state == LITERAL_RUN) {
				slop = op - lastliteral;
				tif->tif_rawcc += lastliteral - tif->tif_rawcp;
				if (!TIFFFlushData1(tif))
					return (-1);
				op = tif->tif_rawcp;
				for (; slop-- > 0; *op++ = *lastliteral++)
					;
				lastliteral = tif->tif_rawcp;
			} else {
				tif->tif_rawcc += op - tif->tif_rawcp;
				if (!TIFFFlushData1(tif))
					return (-1);
				op = tif->tif_rawcp;
			}
		}
		switch (state) {
		case BASE:		/* initial state, set run/literal */
			if (n > 1) {
				state = RUN;
				if (n > 128) {
					*op++ = -127;
					*op++ = b;
					n -= 128;
					goto again;
				}
				*op++ = -(n-1);
				*op++ = b;
			} else {
				lastliteral = op;
				*op++ = 0;
				*op++ = b;
				state = LITERAL;
			}
			break;
		case LITERAL:		/* last object was literal string */
			if (n > 1) {
				state = LITERAL_RUN;
				if (n > 128) {
					*op++ = -127;
					*op++ = b;
					n -= 128;
					goto again;
				}
				*op++ = -(n-1);		/* encode run */
				*op++ = b;
			} else {			/* extend literal */
				if (++(*lastliteral) == 127)
					state = BASE;
				*op++ = b;
			}
			break;
		case RUN:		/* last object was run */
			if (n > 1) {
				if (n > 128) {
					*op++ = -127;
					*op++ = b;
					n -= 128;
					goto again;
				}
				*op++ = -(n-1);
				*op++ = b;
			} else {
				lastliteral = op;
				*op++ = 0;
				*op++ = b;
				state = LITERAL;
			}
			break;
		case LITERAL_RUN:	/* literal followed by a run */
			/*
			 * Check to see if previous run should
			 * be converted to a literal, in which
			 * case we convert literal-run-literal
			 * to a single literal.
			 */
			if (n == 1 && op[-2] == (char)-1 &&
			    *lastliteral < 126) {
				state = (((*lastliteral) += 2) == 127 ?
				    BASE : LITERAL);
				op[-2] = op[-1];	/* replicate */
			} else
				state = RUN;
			goto again;
		}
	}
	tif->tif_rawcc += op - tif->tif_rawcp;
	tif->tif_rawcp = op;
	return (1);
}

static int
PackBitsDecode(tif, op, occ, s)
	TIFF *tif;
	register u_char *op;
	register tiff_u_long occ;
	u_int s;
{
	register char *bp;
	register int n, b;
	register tiff_u_long cc;

	bp = tif->tif_rawcp; cc = tif->tif_rawcc;
	while (cc > 0 && occ > 0) {
		n = (int) *bp++;
		/*
		 * Watch out for compilers that
		 * don't sign extend chars...
		 */
		if (n >= 128)
			n -= 256;
		if (n < 0) {		/* replicate next byte -n+1 times */
			cc--;
			if (n == -128)	/* nop */
				continue;
			n = -n + 1;
			occ -= n;
			for (b = *bp++; n-- > 0;)
				*op++ = b;
		} else {		/* copy next n+1 bytes literally */
			mybcopy(bp, op, ++n);
			op += n; occ -= n;
			bp += n; cc -= n;
		}
	}
	tif->tif_rawcp = bp;
	tif->tif_rawcc = cc;
	if (occ > 0) {
		TIFFError(tif->tif_name,
		    "PackBitsDecode: Not enough data for scanline %d",
		    tif->tif_row);
		return (0);
	}
	/* check for buffer overruns? */
	return (1);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_prnt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <stdio.h>
#include <string.h>

static void
DECLARE3(defaultPHandler,FILE*, fd, char*, fmt, va_list, ap)
{
	vfprintf(fd, fmt, ap);
}

static TIFFPrintHandler _printHandler = defaultPHandler;

TIFFPrintHandler
DECLARE1(TIFFSetPrintHandler, TIFFPrintHandler, handler)
{
	TIFFPrintHandler prev = _printHandler;
	_printHandler = handler;
	return (prev);
}

void
#if USE_PROTOTYPES
TIFFPrint(FILE* fd,char *fmt, ...)
#else
/*VARARGS2*/
TIFFPrint(fd, fmt, va_alist)
	FILE *fd;
	char *fmt;
	va_dcl
#endif
{
	if (_printHandler) {
		va_list ap;
		VA_START(ap, fmt);
		(*_printHandler)(fd, fmt, ap);
		va_end(ap);
	}
}


#ifdef JPEG_SUPPORT
static
JPEGPrintQTable(fd, tab)
	FILE *fd;
	u_char tab[64];
{
	int i, j;
	char *sep;

	fputc('\n', fd);
	for (i = 0; i < 8; i++) {
		sep = "    ";
		for (j = 0; j < 8; j++) {
			TIFFPrint(fd, "%s%2u", sep, tab[8*i+j]);
			sep = ", ";
		}
		fputc('\n', fd);
	}
}

static
JPEGPrintCTable(fd, tab)
	FILE *fd;
	u_char *tab;
{
	int i, n, count;
	char *sep;

	TIFFPrint(fd, "\n    Bits:");
	count = 0;
	for (i = 0; i < 16; i++) {
		TIFFPrint(fd, " %u", tab[i]);
		count += tab[i];
	}
	n = 0;
	for (; count > 0; count--) {
		if ((n % 8) == 0) {
			fputc('\n', fd);
			sep = "    ";
		}
		TIFFPrint(fd, "%s0x%02x", sep, tab[i++]);
		sep = ", ";
		n++;

	}
	if (n % 8)
		fputc('\n', fd);
}
#endif

static const char *photoNames[] = {
    "min-is-white",				/* PHOTOMETRIC_MINISWHITE */
    "min-is-black",				/* PHOTOMETRIC_MINISBLACK */
    "RGB color",				/* PHOTOMETRIC_RGB */
    "palette color (RGB from colormap)",	/* PHOTOMETRIC_PALETTE */
    "transparency mask",			/* PHOTOMETRIC_MASK */
    "separated",				/* PHOTOMETRIC_SEPARATED */
    "YCbCr",					/* PHOTOMETRIC_YCBCR */
    "7 (0x7)",
    "CIE L*a*b*",				/* PHOTOMETRIC_CIELAB */
};
#define	NPHOTONAMES	(sizeof (photoNames) / sizeof (photoNames[0]))

static const char *orientNames[] = {
    "0 (0x0)",
    "row 0 top, col 0 lhs",			/* ORIENTATION_TOPLEFT */
    "row 0 top, col 0 rhs",			/* ORIENTATION_TOPRIGHT */
    "row 0 bottom, col 0 rhs",			/* ORIENTATION_BOTRIGHT */
    "row 0 bottom, col 0 lhs",			/* ORIENTATION_BOTLEFT */
    "row 0 lhs, col 0 top",			/* ORIENTATION_LEFTTOP */
    "row 0 rhs, col 0 top",			/* ORIENTATION_RIGHTTOP */
    "row 0 rhs, col 0 bottom",			/* ORIENTATION_RIGHTBOT */
    "row 0 lhs, col 0 bottom",			/* ORIENTATION_LEFTBOT */
};
#define	NORIENTNAMES	(sizeof (orientNames) / sizeof (orientNames[0]))

/*
 * Print the contents of the current directory
 * to the specified stdio file stream.
 */
void
TIFFPrintDirectory(tif, fd, flags)
	TIFF *tif;
	FILE *fd;
	long flags;
{
	register TIFFDirectory *td;
	char *sep;
	int i, j;
	long n;

	TIFFPrint(fd, "TIFF Directory at offset 0x%x\n", tif->tif_diroff);
	td = &tif->tif_dir;
	if (TIFFFieldSet(tif,FIELD_SUBFILETYPE)) {
		TIFFPrint(fd, "  Subfile Type:");
		sep = " ";
		if (td->td_subfiletype & FILETYPE_REDUCEDIMAGE) {
			TIFFPrint(fd, "%sreduced-resolution image", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_PAGE) {
			TIFFPrint(fd, "%smulti-page document", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_MASK)
			TIFFPrint(fd, "%stransparency mask", sep);
		TIFFPrint(fd, " (%u = 0x%x)\n",
		    td->td_subfiletype, td->td_subfiletype);
	}
	if (TIFFFieldSet(tif,FIELD_IMAGEDIMENSIONS)) {
		TIFFPrint(fd, "  Image Width: %lu Image Length: %lu",
		    td->td_imagewidth, td->td_imagelength);
		if (TIFFFieldSet(tif,FIELD_IMAGEDEPTH))
			TIFFPrint(fd, " Image Depth: %lu", td->td_imagedepth);
		TIFFPrint(fd, "\n");
	}
	if (TIFFFieldSet(tif,FIELD_TILEDIMENSIONS)) {
		TIFFPrint(fd, "  Tile Width: %lu Tile Length: %lu",
		    td->td_tilewidth, td->td_tilelength);
		if (TIFFFieldSet(tif,FIELD_TILEDEPTH))
			TIFFPrint(fd, " Tile Depth: %lu", td->td_tiledepth);
		TIFFPrint(fd, "\n");
	}
	if (TIFFFieldSet(tif,FIELD_RESOLUTION)) {
		TIFFPrint(fd, "  Resolution: %g, %g",
		    td->td_xresolution, td->td_yresolution);
		if (TIFFFieldSet(tif,FIELD_RESOLUTIONUNIT)) {
			switch (td->td_resolutionunit) {
			case RESUNIT_NONE:
				TIFFPrint(fd, " (unitless)");
				break;
			case RESUNIT_INCH:
				TIFFPrint(fd, " pixels/inch");
				break;
			case RESUNIT_CENTIMETER:
				TIFFPrint(fd, " pixels/cm");
				break;
			default:
				TIFFPrint(fd, " (unit %u = 0x%x)",
				    td->td_resolutionunit,
				    td->td_resolutionunit);
				break;
			}
		}
		TIFFPrint(fd, "\n");
	}
	if (TIFFFieldSet(tif,FIELD_POSITION))
		TIFFPrint(fd, "  Position: %g, %g\n",
		    td->td_xposition, td->td_yposition);
	if (TIFFFieldSet(tif,FIELD_BITSPERSAMPLE))
		TIFFPrint(fd, "  Bits/Sample: %u\n", td->td_bitspersample);
	if (TIFFFieldSet(tif,FIELD_SAMPLEFORMAT)) {
		TIFFPrint(fd, "  Sample Format: ");
		switch (td->td_sampleformat) {
		case SAMPLEFORMAT_VOID:
			TIFFPrint(fd, "void\n");
			break;
		case SAMPLEFORMAT_INT:
			TIFFPrint(fd, "signed integer\n");
			break;
		case SAMPLEFORMAT_UINT:
			TIFFPrint(fd, "unsigned integer\n");
			break;
		case SAMPLEFORMAT_IEEEFP:
			TIFFPrint(fd, "IEEE floating point\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_sampleformat, td->td_sampleformat);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_COMPRESSION)) {
		TIFFPrint(fd, "  Compression Scheme: ");
		switch (td->td_compression) {
		case COMPRESSION_NONE:
			TIFFPrint(fd, "none\n");
			break;
		case COMPRESSION_CCITTRLE:
			TIFFPrint(fd, "CCITT modified Huffman encoding\n");
			break;
		case COMPRESSION_CCITTFAX3:
			TIFFPrint(fd, "CCITT Group 3 facsimile encoding\n");
			break;
		case COMPRESSION_CCITTFAX4:
			TIFFPrint(fd, "CCITT Group 4 facsimile encoding\n");
			break;
		case COMPRESSION_CCITTRLEW:
			TIFFPrint(fd, "CCITT modified Huffman encoding %s\n",
			    "w/ word alignment");
			break;
		case COMPRESSION_PACKBITS:
			TIFFPrint(fd, "Macintosh PackBits encoding\n");
			break;
		case COMPRESSION_THUNDERSCAN:
			TIFFPrint(fd, "ThunderScan 4-bit encoding\n");
			break;
		case COMPRESSION_LZW:
			TIFFPrint(fd, "Lempel-Ziv & Welch encoding\n");
			break;
		case COMPRESSION_NEXT:
			TIFFPrint(fd, "NeXT 2-bit encoding\n");
			break;
		case COMPRESSION_JPEG:
			TIFFPrint(fd, "JPEG encoding\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_compression, td->td_compression);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_PHOTOMETRIC)) {
		TIFFPrint(fd, "  Photometric Interpretation: ");
		if (td->td_photometric < NPHOTONAMES)
			TIFFPrint(fd, "%s\n", photoNames[td->td_photometric]);
		else
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_photometric, td->td_photometric);
	}
	if (TIFFFieldSet(tif,FIELD_MATTEING))
		TIFFPrint(fd, "  Matteing: %s\n", td->td_matteing ?
		    "pre-multiplied with alpha channel" : "none");
#ifdef CMYK_SUPPORT
	if (TIFFFieldSet(tif,FIELD_INKSET)) {
		TIFFPrint(fd, "  Ink Set: ");
		switch (td->td_inkset) {
		case INKSET_CMYK:
			TIFFPrint(fd, "CMYK\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_inkset, td->td_inkset);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_INKNAMES)) {
		char *cp;
		TIFFPrint(fd, "  Ink Names: ");
		i = td->td_samplesperpixel;
		sep = "";
		for (cp = td->td_inknames; i > 0; cp = strchr(cp, '\0')) {
			TIFFPrint(fd, "%s%s", sep, cp);
			sep = ", ";
		}
	}
	if (TIFFFieldSet(tif,FIELD_DOTRANGE))
		TIFFPrint(fd, "  Dot Range: %u-%u\n",
		    td->td_dotrange[0], td->td_dotrange[1]);
	if (TIFFFieldSet(tif,FIELD_TARGETPRINTER))
		TIFFPrint(fd, "  Target Printer: %s\n", td->td_targetprinter);
#endif
	if (TIFFFieldSet(tif,FIELD_THRESHHOLDING)) {
		TIFFPrint(fd, "  Thresholding: ");
		switch (td->td_threshholding) {
		case THRESHHOLD_BILEVEL:
			TIFFPrint(fd, "bilevel art scan\n");
			break;
		case THRESHHOLD_HALFTONE:
			TIFFPrint(fd, "halftone or dithered scan\n");
			break;
		case THRESHHOLD_ERRORDIFFUSE:
			TIFFPrint(fd, "error diffused\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_threshholding, td->td_threshholding);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_FILLORDER)) {
		TIFFPrint(fd, "  FillOrder: ");
		switch (td->td_fillorder) {
		case FILLORDER_MSB2LSB:
			TIFFPrint(fd, "msb-to-lsb\n");
			break;
		case FILLORDER_LSB2MSB:
			TIFFPrint(fd, "lsb-to-msb\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_fillorder, td->td_fillorder);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_PREDICTOR)) {
		TIFFPrint(fd, "  Predictor: ");
		switch (td->td_predictor) {
		case 1:
			TIFFPrint(fd, "none\n");
			break;
		case 2:
			TIFFPrint(fd, "horizontal differencing\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_predictor, td->td_predictor);
			break;
		}
	}
#ifdef YCBCR_SUPPORT
	if (TIFFFieldSet(tif,FIELD_YCBCRSUBSAMPLING))
		TIFFPrint(fd, "  YCbCr Subsampling: %u, %u\n",
		    td->td_ycbcrsubsampling[0], td->td_ycbcrsubsampling[1]);
	if (TIFFFieldSet(tif,FIELD_YCBCRPOSITIONING)) {
		TIFFPrint(fd, "  YCbCr Positioning: ");
		switch (td->td_ycbcrpositioning) {
		case YCBCRPOSITION_CENTERED:
			TIFFPrint(fd, "centered\n");
			break;
		case YCBCRPOSITION_COSITED:
			TIFFPrint(fd, "cosited\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_ycbcrpositioning, td->td_ycbcrpositioning);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_YCBCRCOEFFICIENTS))
		TIFFPrint(fd, "  YCbCr Coefficients: %g, %g, %g\n",
		    td->td_ycbcrcoeffs[0],
		    td->td_ycbcrcoeffs[1],
		    td->td_ycbcrcoeffs[2]);
#endif
#ifdef JPEG_SUPPORT
	if (TIFFFieldSet(tif,FIELD_JPEGPROC)) {
		TIFFPrint(fd, "  JPEG Processing Mode: ");
		switch (td->td_jpegproc) {
		case JPEGPROC_BASELINE:
			TIFFPrint(fd, "baseline sequential algorithm\n");
			break;
		case JPEGPROC_LOSSLESS:
			TIFFPrint(fd, "lossless algorithm with Huffman coding\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_jpegproc, td->td_jpegproc);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_JPEGRESTARTINTERVAL)) {
		TIFFPrint(fd, "  JPEG Restart Interval: ");
		if (td->td_jpegrestartinterval)
			TIFFPrint(fd, "%u\n", td->td_jpegrestartinterval);
		else
			TIFFPrint(fd, "(no restart markers)\n");
	}
	if (TIFFFieldSet(tif,FIELD_JPEGQTABLES)) {
		TIFFPrint(fd, "  JPEG Quantization Tables: ");
		if (flags & TIFFPRINT_JPEGQTABLES) {
			for (i = 0; i < td->td_samplesperpixel; i++)
				JPEGPrintQTable(fd, td->td_qtab[i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
	if (TIFFFieldSet(tif,FIELD_JPEGDCTABLES)) {
		TIFFPrint(fd, "  JPEG DC Tables: ");
		if (flags & TIFFPRINT_JPEGDCTABLES) {
			for (i = 0; i < td->td_samplesperpixel; i++)
				JPEGPrintCTable(fd, td->td_dctab[i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
	if (TIFFFieldSet(tif,FIELD_JPEGACTABLES)) {
		TIFFPrint(fd, "  JPEG AC Tables: ");
		if (flags & TIFFPRINT_JPEGACTABLES) {
			for (i = 0; i < td->td_samplesperpixel; i++)
				JPEGPrintCTable(fd, td->td_actab[i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
#endif
	if (TIFFFieldSet(tif,FIELD_HALFTONEHINTS))
		TIFFPrint(fd, "  Halftone Hints: light %u dark %u\n",
		    td->td_halftonehints[0], td->td_halftonehints[1]);
	if (TIFFFieldSet(tif,FIELD_ARTIST))
		TIFFPrint(fd, "  Artist: \"%s\"\n", td->td_artist);
	if (TIFFFieldSet(tif,FIELD_DATETIME))
		TIFFPrint(fd, "  Date & Time: \"%s\"\n", td->td_datetime);
	if (TIFFFieldSet(tif,FIELD_HOSTCOMPUTER))
		TIFFPrint(fd, "  Host Computer: \"%s\"\n", td->td_hostcomputer);
	if (TIFFFieldSet(tif,FIELD_SOFTWARE))
		TIFFPrint(fd, "  Software: \"%s\"\n", td->td_software);
	if (TIFFFieldSet(tif,FIELD_DOCUMENTNAME))
		TIFFPrint(fd, "  Document Name: \"%s\"\n", td->td_documentname);
	if (TIFFFieldSet(tif,FIELD_IMAGEDESCRIPTION))
		TIFFPrint(fd, "  Image Description: \"%s\"\n",
		    td->td_imagedescription);
	if (TIFFFieldSet(tif,FIELD_MAKE))
		TIFFPrint(fd, "  Make: \"%s\"\n", td->td_make);
	if (TIFFFieldSet(tif,FIELD_MODEL))
		TIFFPrint(fd, "  Model: \"%s\"\n", td->td_model);
	if (TIFFFieldSet(tif,FIELD_ORIENTATION)) {
		TIFFPrint(fd, "  Orientation: ");
		if (td->td_orientation < NORIENTNAMES)
			TIFFPrint(fd, "%s\n", orientNames[td->td_orientation]);
		else
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_orientation, td->td_orientation);
	}
	if (TIFFFieldSet(tif,FIELD_SAMPLESPERPIXEL))
		TIFFPrint(fd, "  Samples/Pixel: %u\n", td->td_samplesperpixel);
	if (TIFFFieldSet(tif,FIELD_ROWSPERSTRIP)) {
		TIFFPrint(fd, "  Rows/Strip: ");
		if (td->td_rowsperstrip == 0xffffffffL)
			TIFFPrint(fd, "(infinite)\n");
		else
			TIFFPrint(fd, "%u\n", td->td_rowsperstrip);
	}
	if (TIFFFieldSet(tif,FIELD_MINSAMPLEVALUE))
		TIFFPrint(fd, "  Min Sample Value: %u\n", td->td_minsamplevalue);
	if (TIFFFieldSet(tif,FIELD_MAXSAMPLEVALUE))
		TIFFPrint(fd, "  Max Sample Value: %u\n", td->td_maxsamplevalue);
	if (TIFFFieldSet(tif,FIELD_PLANARCONFIG)) {
		TIFFPrint(fd, "  Planar Configuration: ");
		switch (td->td_planarconfig) {
		case PLANARCONFIG_CONTIG:
			TIFFPrint(fd, "single image plane\n");
			break;
		case PLANARCONFIG_SEPARATE:
			TIFFPrint(fd, "separate image planes\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_planarconfig, td->td_planarconfig);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_PAGENAME))
		TIFFPrint(fd, "  Page Name: \"%s\"\n", td->td_pagename);
	if (TIFFFieldSet(tif,FIELD_GROUP3OPTIONS)) {
		TIFFPrint(fd, "  Group 3 Options:");
		sep = " ";
		if (td->td_group3options & GROUP3OPT_2DENCODING)
			TIFFPrint(fd, "%s2-d encoding", sep), sep = "+";
		if (td->td_group3options & GROUP3OPT_FILLBITS)
			TIFFPrint(fd, "%sEOL padding", sep), sep = "+";
		if (td->td_group3options & GROUP3OPT_UNCOMPRESSED)
			TIFFPrint(fd, "%suncompressed data", sep);
		TIFFPrint(fd, " (%u = 0x%x)\n",
		    td->td_group3options, td->td_group3options);
	}
	if (TIFFFieldSet(tif,FIELD_CLEANFAXDATA)) {
		TIFFPrint(fd, "  Fax Data: ");
		switch (td->td_cleanfaxdata) {
		case CLEANFAXDATA_CLEAN:
			TIFFPrint(fd, "clean\n");
			break;
		case CLEANFAXDATA_REGENERATED:
			TIFFPrint(fd, "receiver regenerated\n");
			break;
		case CLEANFAXDATA_UNCLEAN:
			TIFFPrint(fd, "uncorrected errors\n");
			break;
		default:
			TIFFPrint(fd, "(%u = 0x%x)\n",
			    td->td_cleanfaxdata, td->td_cleanfaxdata);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_BADFAXLINES))
		TIFFPrint(fd, "  Bad Fax Lines: %u\n", td->td_badfaxlines);
	if (TIFFFieldSet(tif,FIELD_BADFAXRUN))
		TIFFPrint(fd, "  Consecutive Bad Fax Lines: %u\n",
		    td->td_badfaxrun);
	if (TIFFFieldSet(tif,FIELD_GROUP4OPTIONS)) {
		TIFFPrint(fd, "  Group 4 Options:");
		if (td->td_group4options & GROUP4OPT_UNCOMPRESSED)
			TIFFPrint(fd, "uncompressed data");
		TIFFPrint(fd, " (%u = 0x%x)\n",
		    td->td_group4options, td->td_group4options);
	}
	if (TIFFFieldSet(tif,FIELD_PAGENUMBER))
		TIFFPrint(fd, "  Page Number: %u-%u\n",
		    td->td_pagenumber[0], td->td_pagenumber[1]);
	if (TIFFFieldSet(tif,FIELD_COLORMAP)) {
		TIFFPrint(fd, "  Color Map: ");
		if (flags & TIFFPRINT_COLORMAP) {
			TIFFPrint(fd, "\n");
			n = 1L<<td->td_bitspersample;
			for (i = 0; i < n; i++)
				TIFFPrint(fd, "   %5d: %5u %5u %5u\n",
				    i,
				    td->td_colormap[0][i],
				    td->td_colormap[1][i],
				    td->td_colormap[2][i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
#ifdef COLORIMETRY_SUPPORT
	if (TIFFFieldSet(tif,FIELD_WHITEPOINT))
		TIFFPrint(fd, "  White Point: %g-%g\n",
		    td->td_whitepoint[0], td->td_whitepoint[1]);
	if (TIFFFieldSet(tif,FIELD_PRIMARYCHROMAS))
		TIFFPrint(fd, "  Primary Chromaticities: %g,%g %g,%g %g,%g\n",
		    td->td_primarychromas[0], td->td_primarychromas[1],
		    td->td_primarychromas[2], td->td_primarychromas[3],
		    td->td_primarychromas[4], td->td_primarychromas[5]);
	if (TIFFFieldSet(tif,FIELD_REFBLACKWHITE)) {
		TIFFPrint(fd, "  Reference Black/White:\n");
		for (i = 0; i < (int)td->td_samplesperpixel; i++)
			TIFFPrint(fd, "    %2d: %5g %5g\n",
			    i,
			    td->td_refblackwhite[2*i+0],
			    td->td_refblackwhite[2*i+1]);
	}
	if (TIFFFieldSet(tif,FIELD_TRANSFERFUNCTION)) {
		TIFFPrint(fd, "  Transfer Function: ");
		if (flags & TIFFPRINT_CURVES) {
			TIFFPrint(fd, "\n");
			n = 1L<<td->td_bitspersample;
			for (i = 0; i < n; i++) {
				TIFFPrint(fd, "    %2d: %5u",
				    i, td->td_transferfunction[0][i]);
				for (j = 1; j < (int)td->td_samplesperpixel; j++)
					TIFFPrint(fd, " %5u",
					    td->td_transferfunction[j][i]);
				putc('\n', fd);
			}
		} else
			TIFFPrint(fd, "(present)\n");
	}
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	{
		long deg,min;
		float sec;

#define GET_DEG_MIN_SEC(s1000) \
			deg=(s1000)/36e5; \
			min=(s1000)/6e4 - deg*60; \
			sec=(float)(s1000)/1000 - deg*3600-min*60;
		
		if (TIFFhasCartoTags(tif))
		{
			/* Print the values in the private IFD */
			if (TIFFFieldSet(tif,FIELD_CARTO_IFD_OFFSET))
				TIFFPrint(fd, "  Cartographic Directory Offset at  0x%x:\n",
					td->td_carto_ifd_offset);
			else
				TIFFPrint(fd, "  Cartographic Data Derived from Old Method:\n");
			TIFFPrint(fd, "  \tProjection Type: %s\n",
				(td->td_projectiontype==0)? "UTM":"???");
			TIFFPrint(fd, "  \tReference X Position: %lu pixels from left\n",
				td->td_proj_xpos);
			TIFFPrint(fd, "  \tReference Y Position: %lu pixels from top\n",
				td->td_proj_ypos);
			GET_DEG_MIN_SEC(td->td_latitude)
			TIFFPrint(fd, "  \tReference Latitude  : (%d Deg, %d Min, %g Sec) from S. Pole\n",
				deg,min,sec);
			GET_DEG_MIN_SEC(td->td_longitude)
			TIFFPrint(fd, "  \tReference Longitude : (%d Deg, %d Min, %g Sec) from -180.\n",
				deg,min,sec);
			GET_DEG_MIN_SEC((1/td->td_xpixperangle))
			TIFFPrint(fd, "  \tX Map Resolution:     (%d Deg, %d Min, %g Sec) per Pixel\n",
				deg,min,sec);
			GET_DEG_MIN_SEC((1/td->td_ypixperangle))
			TIFFPrint(fd, "  \tY Map Resolution:     (%d Deg, %d Min, %g Sec) per Pixel\n",
				deg,min,sec);
		}
	}
#endif
	if ((flags & TIFFPRINT_STRIPS) &&
	    TIFFFieldSet(tif,FIELD_STRIPOFFSETS)) {
		TIFFPrint(fd, "  %u %s:\n",
		    td->td_nstrips,
		    isTiled(tif) ? "Tiles" : "Strips");
		for (i = 0; i < td->td_nstrips; i++)
			TIFFPrint(fd, "    %3d: [%8u, %8u]\n",
			    i, td->td_stripoffset[i], td->td_stripbytecount[i]);
	}
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_strp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_strip.c,v 1.7 92/02/10 19:06:42 sam Exp $";
#endif

/*
 * Copyright (c) 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * Strip-organized Image Support Routines.
 */
#include "tiffiop.h"

/*
 * Compute which strip a (row,sample) value is in.
 */
u_int
TIFFComputeStrip(tif, row, sample)
	TIFF *tif;
	tiff_u_long row;
	u_int sample;
{
	TIFFDirectory *td = &tif->tif_dir;
	u_int strip;

	strip = row / td->td_rowsperstrip;
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE) {
		if (sample >= td->td_samplesperpixel) {
			TIFFError(tif->tif_name,
			    "%d: Sample out of range, max %d",
			    sample, td->td_samplesperpixel);
			return (0);
		}
		strip += sample*td->td_stripsperimage;
	}
	return (strip);
}

/*
 * Compute how many strips are in an image.
 */
u_int
TIFFNumberOfStrips(tif)
	TIFF *tif;
{
	TIFFDirectory *td = &tif->tif_dir;

	return (td->td_rowsperstrip == 0xffffffff ?
	     (td->td_imagelength != 0 ? 1 : 0) :
	     (u_int)howmany(td->td_imagelength, td->td_rowsperstrip));
}

/*
 * Compute the # bytes in a variable height, row-aligned strip.
 */
tiff_u_long
TIFFVStripSize(tif, nrows)
	TIFF *tif;
	tiff_u_long nrows;
{
	TIFFDirectory *td = &tif->tif_dir;

	if (nrows == (tiff_u_long)-1)
		nrows = td->td_imagelength;
#ifdef YCBCR_SUPPORT
	if (td->td_planarconfig == PLANARCONFIG_CONTIG &&
	    td->td_photometric == PHOTOMETRIC_YCBCR) {
		/*
		 * Packed YCbCr data contain one Cb+Cr for every
		 * HorizontalSampling*VerticalSampling Y values.
		 * Must also roundup width and height when calculating
		 * since images that are not a multiple of the
		 * horizontal/vertical subsampling area include
		 * YCbCr data for the extended image.
		 */
		tiff_u_long w =
		    roundup(td->td_imagewidth, td->td_ycbcrsubsampling[0]);
		tiff_u_long scanline = howmany(w*td->td_bitspersample, 8);
		tiff_u_long samplingarea =
		    td->td_ycbcrsubsampling[0]*td->td_ycbcrsubsampling[1];
		nrows = roundup(nrows, td->td_ycbcrsubsampling[1]);
		/* NB: don't need howmany here 'cuz everything is rounded */
		return (nrows*scanline + 2*(nrows*scanline / samplingarea));
	} else
#endif
		return (nrows * TIFFScanlineSize(tif));
}

/*
 * Compute the # bytes in a (row-aligned) strip.
 */
tiff_u_long
TIFFStripSize(tif)
	TIFF *tif;
{
	return (TIFFVStripSize(tif, tif->tif_dir.td_rowsperstrip));
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_swab.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_swab.c,v 1.12 92/02/10 19:06:03 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

/*
 * TIFF Library Bit & Byte Swapping Support.
 *
 * XXX We assume short = 16-bits and long = 32-bits XXX
 *   XXX Disaster for transputers! shorts are 32 bits !!!
 *   Fix installed for transputers.
 */

#include "tiffio.h"

#ifndef TIFFSwabShort
#ifdef transputer
TIFFSwabShort(wp)
	unsigned short *wp;
{
	register unsigned char *cp = (unsigned char *)wp;
	int t;

	  /* swap both pairs of bytes */
	t = cp[1]; cp[1] = cp[0]; cp[0] = t;
	t = cp[3]; cp[3] = cp[2]; cp[2] = t;
}
#else
TIFFSwabShort(wp)
	unsigned short *wp;
{
	register unsigned char *cp = (unsigned char *)wp;
	int t;

	t = cp[1]; cp[1] = cp[0]; cp[0] = t;
}
#endif
#endif

#ifndef TIFFSwabLong
TIFFSwabLong(lp)
	tiff_u_long *lp;
{
	register unsigned char *cp = (unsigned char *)lp;
	int t;

	t = cp[3]; cp[3] = cp[0]; cp[0] = t;
	t = cp[2]; cp[2] = cp[1]; cp[1] = t;
}
#endif

#ifndef TIFFSwabArrayOfShort
#ifdef transputer
TIFFSwabArrayOfShort(wp, n)
	unsigned short *wp;
	register int n;
{
	register unsigned char *cp;
	register int t;

	/* XXX unroll loop some */
	while (n-- > 0) {
		cp = (unsigned char *)wp;
		t = cp[1]; cp[1] = cp[0]; cp[0] = t;
		t = cp[2]; cp[2] = cp[3]; cp[3] = t;
		wp++;
	}
}
#else
TIFFSwabArrayOfShort(wp, n)
	unsigned short *wp;
	register int n;
{
	register unsigned char *cp;
	register int t;

	/* XXX unroll loop some */
	while (n-- > 0) {
		cp = (unsigned char *)wp;
		t = cp[1]; cp[1] = cp[0]; cp[0] = t;
		wp++;
	}
}
#endif /* transputer */
#endif

#ifndef TIFFSwabArrayOfLong
TIFFSwabArrayOfLong(lp, n)
	register tiff_u_long *lp;
	register int n;
{
	register unsigned char *cp;
	register int t;

	/* XXX unroll loop some */
	while (n-- > 0) {
		cp = (unsigned char *)lp;
		t = cp[3]; cp[3] = cp[0]; cp[0] = t;
		t = cp[2]; cp[2] = cp[1]; cp[1] = t;
		lp++;
	}
}
#endif

/*
 * Bit reversal tables.  TIFFBitRevTable[<byte>] gives
 * the bit reversed value of <byte>.  Used in various
 * places in the library when the FillOrder requires
 * bit reversal of byte values (e.g. CCITT Fax 3
 * encoding/decoding).  TIFFNoBitRevTable is provided
 * for algorithms that want an equivalent table that
 * do not reverse bit values.
 */
#if defined(__STDC__) || defined(__EXTENDED__) || USE_CONST
const unsigned char TIFFBitRevTable[256] = {
#else
unsigned char TIFFBitRevTable[256] = {
#endif
    0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
    0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
    0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
    0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
    0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
    0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
    0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
    0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
    0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
    0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
    0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
    0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
    0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
    0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
    0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
    0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
    0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
    0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
    0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
    0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
    0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
    0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
    0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
    0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
    0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
    0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
    0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
    0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
    0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
    0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
    0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
    0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};
#if defined(__STDC__) || defined(__EXTENDED__) || USE_CONST
const unsigned char TIFFNoBitRevTable[256] = {
#else
unsigned char TIFFNoBitRevTable[256] = {
#endif
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 
    0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 
    0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 
    0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 
    0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 
    0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 
    0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 
    0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 
    0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 
    0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 
    0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 
    0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 
    0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 
    0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 
    0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 
    0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf, 
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 
    0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef, 
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 
    0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff, 
};

TIFFReverseBits(cp, n)
	register unsigned char *cp;
	register int n;
{
	for (; n > 8; n -= 8) {
		cp[0] = TIFFBitRevTable[cp[0]];
		cp[1] = TIFFBitRevTable[cp[1]];
		cp[2] = TIFFBitRevTable[cp[2]];
		cp[3] = TIFFBitRevTable[cp[3]];
		cp[4] = TIFFBitRevTable[cp[4]];
		cp[5] = TIFFBitRevTable[cp[5]];
		cp[6] = TIFFBitRevTable[cp[6]];
		cp[7] = TIFFBitRevTable[cp[7]];
		cp += 8;
	}
	while (n-- > 0)
		*cp = TIFFBitRevTable[*cp], cp++;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_tile.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_tile.c,v 1.9 92/02/10 19:06:47 sam Exp $";
#endif

/*
 * Copyright (c) 1991, 1992 Sam Leffler
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

/*
 * TIFF Library.
 *
 * Tiled Image Support Routines.
 */
#include "tiffiop.h"

/*
 * Compute which tile an (x,y,z,s) value is in.
 */
u_int
TIFFComputeTile(tif, x, y, z, s)
	TIFF *tif;
	tiff_u_long x, y, z;
	u_int s;
{
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long dx = td->td_tilewidth;
	tiff_u_long dy = td->td_tilelength;
	tiff_u_long dz = td->td_tiledepth;
	u_int tile = 1;

	if (td->td_imagedepth == 1)
		z = 0;
	if (dx == (tiff_u_long) -1)
		dx = td->td_imagewidth;
	if (dy == (tiff_u_long) -1)
		dy = td->td_imagelength;
	if (dz == (tiff_u_long) -1)
		dz = td->td_imagedepth;
	if (dx != 0 && dy != 0 && dz != 0) {
		u_int xpt = howmany(td->td_imagewidth, dx); 
		u_int ypt = howmany(td->td_imagelength, dy); 
		u_int zpt = howmany(td->td_imagedepth, dz); 

		if (td->td_planarconfig == PLANARCONFIG_SEPARATE) 
			tile = (xpt*ypt*zpt)*s +
			     (xpt*ypt)*(z/dz) +
			     xpt*(y/dy) +
			     x/dx;
		else
			tile = (xpt*ypt)*(z/dz) + xpt*(y/dy) + x/dx + s;
	}
	return (tile);
}

/*
 * Check an (x,y,z,s) coordinate
 * against the image bounds.
 */
TIFFCheckTile(tif, x, y, z, s)
	TIFF *tif;
	tiff_u_long x, y, z;
	u_int s;
{
	TIFFDirectory *td = &tif->tif_dir;

	if (x >= td->td_imagewidth) {
		TIFFError(tif->tif_name, "Col %d out of range, max %d",
		    x, td->td_imagewidth);
		return (0);
	}
	if (y >= td->td_imagelength) {
		TIFFError(tif->tif_name, "Row %d out of range, max %d",
		    y, td->td_imagelength);
		return (0);
	}
	if (z >= td->td_imagedepth) {
		TIFFError(tif->tif_name, "Depth %d out of range, max %d",
		    z, td->td_imagedepth);
		return (0);
	}
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE &&
	    s >= td->td_samplesperpixel) {
		TIFFError(tif->tif_name, "Sample %d out of range, max %d",
		    s, td->td_samplesperpixel);
		return (0);
	}
	return (1);
}

/*
 * Compute how many tiles are in an image.
 */
u_int
TIFFNumberOfTiles(tif)
	TIFF *tif;
{
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long dx = td->td_tilewidth;
	tiff_u_long dy = td->td_tilelength;
	tiff_u_long dz = td->td_tiledepth;
	u_int ntiles;

	if (dx == (tiff_u_long) -1)
		dx = td->td_imagewidth;
	if (dy == (tiff_u_long) -1)
		dy = td->td_imagelength;
	if (dz == (tiff_u_long) -1)
		dz = td->td_imagedepth;
	ntiles = (dx != 0 && dy != 0 && dz != 0) ?
	    (howmany(td->td_imagewidth, dx) * howmany(td->td_imagelength, dy) *
		howmany(td->td_imagedepth, dz)) :
	    0;
	return (ntiles);
}

/*
 * Compute the # bytes in each row of a tile.
 */
tiff_u_long
TIFFTileRowSize(tif)
	TIFF *tif;
{
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long rowsize;
	
	if (td->td_tilelength == 0 || td->td_tilewidth == 0)
		return (0);
	rowsize = td->td_bitspersample * td->td_tilewidth;
	if (td->td_planarconfig == PLANARCONFIG_CONTIG)
		rowsize *= td->td_samplesperpixel;
	return (howmany(rowsize, 8));
}

/*
 * Compute the # bytes in a variable length, row-aligned tile.
 */
tiff_u_long
TIFFVTileSize(tif, nrows)
	TIFF *tif;
	tiff_u_long nrows;
{
	TIFFDirectory *td = &tif->tif_dir;
	tiff_u_long tilesize;

	if (td->td_tilelength == 0 || td->td_tilewidth == 0 ||
	    td->td_tiledepth == 0)
		return (0);
#ifdef YCBCR_SUPPORT
	if (td->td_planarconfig == PLANARCONFIG_CONTIG &&
	    td->td_photometric == PHOTOMETRIC_YCBCR) {
		/*
		 * Packed YCbCr data contain one Cb+Cr for every
		 * HorizontalSampling*VerticalSampling Y values.
		 * Must also roundup width and height when calculating
		 * since images that are not a multiple of the
		 * horizontal/vertical subsampling area include
		 * YCbCr data for the extended image.
		 */
		tiff_u_long w =
		    roundup(td->td_tilewidth, td->td_ycbcrsubsampling[0]);
		tiff_u_long rowsize = howmany(w*td->td_bitspersample, 8);
		tiff_u_long samplingarea =
		    td->td_ycbcrsubsampling[0]*td->td_ycbcrsubsampling[1];
		nrows = roundup(nrows, td->td_ycbcrsubsampling[1]);
		/* NB: don't need howmany here 'cuz everything is rounded */
		tilesize = nrows*rowsize + 2*(nrows*rowsize / samplingarea);
	} else
#endif
		tilesize = nrows * TIFFTileRowSize(tif);
	return (tilesize * td->td_tiledepth);
}

/*
 * Compute the # bytes in a row-aligned tile.
 */
tiff_u_long
TIFFTileSize(tif)
	TIFF *tif;
{
	return (TIFFVTileSize(tif, tif->tif_dir.td_tilelength));
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t_vers.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * Copyright (c) 1992 Sam Leffler
 * Copyright (c) 1992 Silicon Graphics, Inc.
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
#include "tiffcompat.h"

const char TIFFVersion[] = "\
LIBTIFF, Version 3.0 BETA\n\
Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler\n\
Copyright (c) 1991, 1992 Silicon Graphics, Inc.\
 ";
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create prototypes.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* $Header: /d/sam/tiff/libtiff/RCS/prototypes.h,v 1.8 92/02/18 18:20:08 sam Exp $ */

/*
 * Copyright (c) 1991, 1992 Sam Leffler
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

#if USE_PROTOTYPES
#define	DECLARE1(f,t1,a1)		f(t1 a1)
#define	DECLARE2(f,t1,a1,t2,a2)		f(t1 a1, t2 a2)
#define	DECLARE3(f,t1,a1,t2,a2,t3,a3)	f(t1 a1, t2 a2, t3 a3)
#define	DECLARE4(f,t1,a1,t2,a2,t3,a3,t4,a4)\
	f(t1 a1, t2 a2, t3 a3, t4 a4)
#define	DECLARE5(f,t1,a1,t2,a2,t3,a3,t4,a4,t5,a5)\
	f(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5)
#define	DECLARE6(f,t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6)\
	f(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6)
#define	DECLARE1V(f,t1,a1)		f(t1 a1 ...)
#define	DECLARE2V(f,t1,a1,t2,a2)	f(t1 a1, t2 a2, ...)
#define	DECLARE3V(f,t1,a1,t2,a2,t3,a3)	f(t1 a1, t2 a2, t3 a3, ...)
#else
#define	DECLARE1(f,t1,a1)		f(a1) t1 a1;
#define	DECLARE2(f,t1,a1,t2,a2)		f(a1,a2) t1 a1; t2 a2;
#define	DECLARE3(f,t1,a1,t2,a2,t3,a3)	f(a1, a2, a3) t1 a1; t2 a2; t3 a3;
#define	DECLARE4(f,t1,a1,t2,a2,t3,a3,t4,a4) \
	f(a1, a2, a3, a4) t1 a1; t2 a2; t3 a3; t4 a4;
#define	DECLARE5(f,t1,a1,t2,a2,t3,a3,t4,a4,t5,a5)\
	f(a1, a2, a3, a4, a5) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5;
#define	DECLARE6(f,t1,a1,t2,a2,t3,a3,t4,a4,t5,a5,t6,a6)\
	f(a1, a2, a3, a4, a5, a6) t1 a1; t2 a2; t3 a3; t4 a4; t5 a5; t6 a6;
#if USE_VARARGS
#define	DECLARE1V(f,t1,a1) \
	f(a1, va_alist) t1 a1; va_dcl
#define	DECLARE2V(f,t1,a1,t2,a2) \
	f(a1, a2, va_alist) t1 a1; t2 a2; va_dcl
#define	DECLARE3V(f,t1,a1,t2,a2,t3,a3) \
	f(a1, a2, a3, va_alist) t1 a1; t2 a2; t3 a3; va_dcl
#else
"Help, I don't know how to handle this case: !USE_PROTOTYPES and !USE_VARARGS?"
#endif
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tiffcompat.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tiffiop.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* $Header: /usr/people/sam/tiff/libtiff/RCS/tiffiop.h,v 1.28 92/03/06 11:59:52 sam Exp $ */

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

#ifndef _TIFFIOP_
#define	_TIFFIOP_
/*
 * ``Library-private'' definitions.
 */
#include "tiffcompat.h"
#include "tiff.h"


/*
 * Internal format of a TIFF directory entry.
 */
typedef	struct {
	tiff_u_long	td_fieldsset[3];	/* bit vector of fields that are set */

	tiff_u_long	td_imagewidth, td_imagelength, td_imagedepth;
	tiff_u_long	td_tilewidth, td_tilelength, td_tiledepth;
	u_short	td_subfiletype;
	u_short	td_bitspersample;
	u_short	td_sampleformat;
	u_short	td_compression;
	u_short	td_photometric;
	u_short	td_threshholding;
	u_short	td_fillorder;
	u_short	td_orientation;
	u_short	td_samplesperpixel;
	u_short	td_predictor;
	tiff_u_long	td_rowsperstrip;
	tiff_u_long	td_minsamplevalue, td_maxsamplevalue;	/* XXX */
	float	td_xresolution, td_yresolution;
	u_short	td_resolutionunit;
	u_short	td_planarconfig;
	float	td_xposition, td_yposition;
	tiff_u_long	td_group3options;
	tiff_u_long	td_group4options;
	u_short	td_pagenumber[2];
	u_short	td_matteing;
	u_short	td_cleanfaxdata;
	u_short	td_badfaxrun;
	tiff_u_long	td_badfaxlines;
	u_short	*td_colormap[3];
	u_short	td_halftonehints[2];
	char	*td_documentname;
	char	*td_artist;
	char	*td_datetime;
	char	*td_hostcomputer;
	char	*td_imagedescription;
	char	*td_make;
	char	*td_model;
	char	*td_software;
	char	*td_pagename;
	tiff_u_long	td_stripsperimage;
	tiff_u_long	td_nstrips;		/* size of offset & bytecount arrays */
	tiff_u_long	*td_stripoffset;
	tiff_u_long	*td_stripbytecount;
#ifdef YCBCR_SUPPORT
	float	*td_ycbcrcoeffs;
	u_short	td_ycbcrsubsampling[2];
	u_short	td_ycbcrpositioning;
#endif
#ifdef JPEG_SUPPORT
	u_short	td_jpegproc;
	u_short	td_jpegrestartinterval;
	u_char	**td_qtab;
	u_char	**td_dctab;
	u_char	**td_actab;
#endif
#ifdef COLORIMETRY_SUPPORT
	float	*td_whitepoint;
	float	*td_primarychromas;
	float	*td_refblackwhite;
	u_short	*td_transferfunction[4];
#endif
#ifdef CMYK_SUPPORT
	u_short	td_inkset;
	u_short	td_dotrange[2];
	char	*td_inknames;
	char	*td_targetprinter;
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	tiff_u_long	td_carto_ifd_offset; /* the only public tag */
	u_short	td_projectiontype;
	tiff_u_long	td_proj_xpos;
	tiff_u_long	td_proj_ypos;
	tiff_u_long	td_latitude;
	tiff_u_long	td_longitude;
	float	td_xpixperangle;
	float	td_ypixperangle;
#endif
} TIFFDirectory;

/*
 * Field flags used to indicate fields that have
 * been set in a directory, and to reference fields
 * when manipulating a directory.
 */
/* multi-entry fields */
#define	FIELD_IMAGEDIMENSIONS		0
#define FIELD_TILEDIMENSIONS		1
#define	FIELD_CELLDIMENSIONS		2		/* XXX */
#define	FIELD_RESOLUTION		3
#define	FIELD_POSITION			4
/* single-entry fields */
#define	FIELD_SUBFILETYPE		5
#define	FIELD_BITSPERSAMPLE		6
#define	FIELD_COMPRESSION		7
#define	FIELD_PHOTOMETRIC		8
#define	FIELD_THRESHHOLDING		9
#define	FIELD_FILLORDER			10
#define	FIELD_DOCUMENTNAME		11
#define	FIELD_IMAGEDESCRIPTION		12
#define	FIELD_MAKE			13
#define	FIELD_MODEL			14
#define	FIELD_ORIENTATION		15
#define	FIELD_SAMPLESPERPIXEL		16
#define	FIELD_ROWSPERSTRIP		17
#define	FIELD_MINSAMPLEVALUE		18
#define	FIELD_MAXSAMPLEVALUE		19
#define	FIELD_PLANARCONFIG		20
#define	FIELD_PAGENAME			21
#define	FIELD_GROUP3OPTIONS		22
#define	FIELD_GROUP4OPTIONS		23
#define	FIELD_RESOLUTIONUNIT		24
#define	FIELD_PAGENUMBER		25
#define	FIELD_STRIPBYTECOUNTS		26
#define	FIELD_STRIPOFFSETS		27
#define	FIELD_COLORMAP			28
#define FIELD_PREDICTOR			29
#define FIELD_ARTIST			30
#define FIELD_DATETIME			31
#define FIELD_HOSTCOMPUTER		32
#define FIELD_SOFTWARE			33
#define	FIELD_MATTEING			34
#define	FIELD_BADFAXLINES		35
#define	FIELD_CLEANFAXDATA		36
#define	FIELD_BADFAXRUN			37
#define FIELD_SAMPLEFORMAT		38
#define	FIELD_SMINSAMPLEVALUE		39
#define	FIELD_SMAXSAMPLEVALUE		40
#define FIELD_IMAGEDEPTH		41
#define FIELD_TILEDEPTH			42
#define	FIELD_HALFTONEHINTS		43
#ifdef YCBCR_SUPPORT
#define FIELD_YCBCRCOEFFICIENTS		44
#define FIELD_YCBCRSUBSAMPLING		45
#define FIELD_YCBCRPOSITIONING		46
#endif
#ifdef JPEG_SUPPORT
#define FIELD_JPEGPROC			47
#define FIELD_JPEGRESTARTINTERVAL	48
#define FIELD_JPEGQTABLES		49
#define FIELD_JPEGDCTABLES		50
#define FIELD_JPEGACTABLES		51
#endif
#ifdef COLORIMETRY_SUPPORT
#define	FIELD_REFBLACKWHITE		52
#define	FIELD_WHITEPOINT		53
#define	FIELD_PRIMARYCHROMAS		54
#define	FIELD_TRANSFERFUNCTION		55
#endif
#ifdef CMYK_SUPPORT
#define	FIELD_INKSET			56
#define	FIELD_INKNAMES			57
#define	FIELD_DOTRANGE			58
#define	FIELD_TARGETPRINTER		59
#endif
#ifdef CARTOGRAPHIC_SUPPORT
#define	FIELD_CARTO_IFD_OFFSET	60   /* The only cart field that is public */
#endif
#define	FIELD_LAST				61

/* Private Cartographic fields */
#ifdef CARTOGRAPHIC_SUPPORT
#define CARTFIELD_FIRST FIELD_LAST+1
#define	FIELD_PROJECTIONTYPE	CARTFIELD_FIRST
#define	FIELD_PROJ_XPOS			CARTFIELD_FIRST+1	
#define	FIELD_PROJ_YPOS			CARTFIELD_FIRST+2	
#define	FIELD_LATITUDE			CARTFIELD_FIRST+3	
#define	FIELD_LONGITUDE			CARTFIELD_FIRST+4	
#define	FIELD_XPIXPERANGLE		CARTFIELD_FIRST+5	
#define	FIELD_YPIXPERANGLE		CARTFIELD_FIRST+6
#define CARTFIELD_LAST			CARTFIELD_FIRST+7
#endif /* CARTO */

#define	TIFFExtractData(tif, type, v) \
    ((tif)->tif_header.tiff_magic == TIFF_BIGENDIAN ? \
        ((v) >> (tif)->tif_typeshift[type]) & (tif)->tif_typemask[type] : \
	(v) & (tif)->tif_typemask[type])
#define	TIFFInsertData(tif, type, v) \
    ((tif)->tif_header.tiff_magic == TIFF_BIGENDIAN ? \
        ((v) & (tif)->tif_typemask[type]) << (tif)->tif_typeshift[type] : \
	(v) & (tif)->tif_typemask[type])

typedef	struct {
	u_short	field_tag;		/* field's tag */
	short	field_readcount;	/* read count (-1 for unknown) */
	short	field_writecount;	/* write count (-1 for unknown) */
	TIFFDataType field_type;	/* type of associated data */
	u_short	field_bit;		/* bit in fieldsset bit vector */
	u_short	field_oktochange;	/* if true, can change while writing */
	char	*field_name;		/* ASCII name */
} TIFFFieldInfo;

#define	FIELD_IGNORE	((u_short)-1)	/* tags processed but ignored */

#define	TIFF_ANY	TIFF_NOTYPE	/* for field descriptor searching */
#define	TIFF_VARIABLE	-1		/* marker for variable length tags */
#define	TIFF_SPP	-2		/* marker for SamplesPerPixel tags */

extern	const TIFFFieldInfo tiffFieldInfo[];/* table of field descriptors */
extern	const int tiffDataWidth[];	/* table of tag datatype widths */
#ifdef CARTOGRAPHIC_SUPPORT
extern	const TIFFFieldInfo cartFieldInfo[];/* table of field descriptors */
#endif

#define BITn(n)				(((unsigned long)1L)<<((n)&0x1f)) 
#define BITFIELDn(tif, n)		((tif)->tif_dir.td_fieldsset[(n)/32]) 
#define TIFFFieldSet(tif, field)	(BITFIELDn(tif, field) & BITn(field)) 
#define TIFFSetFieldBit(tif, field)	(BITFIELDn(tif, field) |= BITn(field))
#define TIFFClrFieldBit(tif, field)	(BITFIELDn(tif, field) &= ~BITn(field))

#define	FieldSet(fields, f)		(fields[(f)/32] & BITn(f))
#define	ResetFieldBit(fields, f)	(fields[(f)/32] &= ~BITn(f))

struct tiff {
	char	*tif_name;		/* name of open file */
	short	tif_fd;			/* open file descriptor */
	short	tif_mode;		/* open mode (O_*) */
	char	tif_fillorder;		/* natural bit fill order for machine */
	char	tif_options;		/* compression-specific options */
	short	tif_flags;
#define	TIFF_DIRTYHEADER	0x1	/* header must be written on close */
#define	TIFF_DIRTYDIRECT	0x2	/* current directory must be written */
#define	TIFF_BUFFERSETUP	0x4	/* data buffers setup */
#define	TIFF_BEENWRITING	0x8	/* written 1+ scanlines to file */
#define	TIFF_SWAB		0x10	/* byte swap file information */
#define	TIFF_NOBITREV		0x20	/* inhibit bit reversal logic */
#define	TIFF_MYBUFFER		0x40	/* my raw data buffer; free on close */
#define	TIFF_ISTILED		0x80	/* file is tile, not strip- based */
#define	TIFF_MAPPED		0x100	/* file is mapped into memory */
#define	TIFF_POSTENCODE		0x200	/* need call to postencode routine */
#ifdef CARTOGRAPHIC_SUPPORT
#define	TIFF_DIRTYCART		0x400	/* CART Directory must be written */
#define	TIFF_HASCARTTAGS	0x800	/* CART Information Present */
#endif
	long	tif_diroff;		/* file offset of current directory */
	long	tif_nextdiroff;		/* file offset of following directory */
	TIFFDirectory tif_dir;		/* internal rep of current directory */
	TIFFHeader tif_header;		/* file's header block */
	int const *tif_typeshift;	/* data type shift counts */
	long const *tif_typemask;	/* data type masks */
	long	tif_row;		/* current scanline */
	int	tif_curdir;		/* current directory (index) */
	int	tif_curstrip;		/* current strip for read/write */
	long	tif_curoff;		/* current offset for read/write */
/* tiling support */
	long 	tif_col;		/* current column (offset by row too) */
	int 	tif_curtile;		/* current tile for read/write */
	long 	tif_tilesize;		/* # of bytes in a tile */
/* compression scheme hooks */
#if USE_PROTOTYPES
	int	(*tif_predecode)(struct tiff *tif);						/* pre row/strip/tile decoding */
	int	(*tif_preencode)(struct tiff *tif);						/* pre row/strip/tile encoding */
	int	(*tif_postencode)(struct tiff *tif);					/* post row/strip/tile encoding */
	int	(*tif_decoderow)(struct tiff *, u_char*, tiff_u_long, u_int);	/* scanline decoding routine */
	int	(*tif_encoderow)(struct tiff *, u_char*, tiff_u_long, u_int);	/* scanline encoding routine */
	int	(*tif_decodestrip)(struct tiff *, u_char*, tiff_u_long, u_int);	/* strip decoding routine */
	int	(*tif_encodestrip)(struct tiff *, u_char*, tiff_u_long, u_int);	/* strip encoding routine */
	int	(*tif_decodetile)(struct tiff *, u_char*, tiff_u_long, u_int);	/* tile decoding routine */
	int	(*tif_encodetile)(struct tiff *, u_char*, tiff_u_long, u_int);	/* tile encoding routine */
	int	(*tif_close)(struct tiff *tif);							/* cleanup-on-close routine */
	int	(*tif_seek)(struct tiff *tif, int);						/* position within a strip routine */
	int	(*tif_cleanup)(struct tiff *tif);						/* routine called to cleanup state */
#else
	int	(*tif_predecode)();	/* pre row/strip/tile decoding */
	int	(*tif_preencode)();	/* pre row/strip/tile encoding */
	int	(*tif_postencode)();	/* post row/strip/tile encoding */
	int	(*tif_decoderow)();	/* scanline decoding routine */
	int	(*tif_encoderow)();	/* scanline encoding routine */
	int	(*tif_decodestrip)();	/* strip decoding routine */
	int	(*tif_encodestrip)();	/* strip encoding routine */
	int	(*tif_decodetile)();	/* tile decoding routine */
	int	(*tif_encodetile)();	/* tile encoding routine */
	int	(*tif_close)();		/* cleanup-on-close routine */
	int	(*tif_seek)();		/* position within a strip routine */
	int	(*tif_cleanup)();	/* routine called to cleanup state */
#endif /* PROTOTYPES */
	char	*tif_data;		/* compression scheme private data */
/* input/output buffering */
	tiff_u_long	tif_scanlinesize;	/* # of bytes in a scanline */
	int	tif_scanlineskew;	/* scanline skew for reading strips */
	char	*tif_rawdata;		/* raw data buffer */
	long	tif_rawdatasize;	/* # of bytes in raw data buffer */
	char	*tif_rawcp;		/* current spot in raw buffer */
	long	tif_rawcc;		/* bytes unread from raw buffer */
#ifdef MMAP_SUPPORT
	char*	tif_base;		/* base of mapped file */
	long	tif_size;		/* size of mapped file region (bytes) */
#endif
};

#define	isTiled(tif)	(((tif)->tif_flags & TIFF_ISTILED) != 0)
#ifdef MMAP_SUPPORT
#define	isMapped(tif)	(((tif)->tif_flags & TIFF_MAPPED) != 0)
#else
#define	isMapped(tif)	0		/* force dead code */
#endif

/* generic option bit names */
#define	TIFF_OPT0	0x1
#define	TIFF_OPT1	0x2
#define	TIFF_OPT2	0x4
#define	TIFF_OPT3	0x8
#define	TIFF_OPT4	0x10
#define	TIFF_OPT5	0x20
#define	TIFF_OPT6	0x40
#define	TIFF_OPT7	0x80

#include "tiffio.h"

/* NB: the tiff_u_long casts are to silence certain ANSI-C compilers */
#ifdef howmany
#undef howmany
#endif
#define	howmany(x, y)	((((tiff_u_long)(x))+(((tiff_u_long)(y))-1))/((tiff_u_long)(y)))
#ifdef roundup
#undef roundup
#endif
#define	roundup(x, y)	(howmany(x,y)*((tiff_u_long)(y)))

#if defined(c_plusplus) || defined(__cplusplus) || defined(__STDC__) || USE_PROTOTYPES
#if defined(__cplusplus)
extern "C" {
#endif
extern  int TIFFWriteTIFFHdr( int fd, TIFFHeader *hdr );
extern	TIFFFieldInfo const *TIFFFindFieldInfo(u_short, TIFFDataType);
extern	TIFFFieldInfo const *TIFFFieldWithTag(u_short);
extern	int TIFFFlushData1(TIFF*);
extern	int TIFFSetCompressionScheme(TIFF*, int);
extern	int _TIFFgetfield(TIFFDirectory*, int, ...);
extern	int TIFFNoRowEncode(TIFF*, u_char*, tiff_u_long, u_int);
extern	int TIFFNoStripEncode(TIFF*, u_char*, tiff_u_long, u_int);
extern	int TIFFNoTileEncode(TIFF*, u_char*, tiff_u_long, u_int);
extern	int TIFFNoRowDecode(TIFF*, u_char*, tiff_u_long, u_int);
extern	int TIFFNoStripDecode(TIFF*, u_char*, tiff_u_long, u_int);
extern	int TIFFNoTileDecode(TIFF*, u_char*, tiff_u_long, u_int);
extern	int TIFFNoDecode(TIFF *, char *);
#if defined(__cplusplus)
}
#endif
#else
extern  int TIFFWriteTIFFHdr();
extern	TIFFFieldInfo const *TIFFFindFieldInfo();
extern	TIFFFieldInfo const *TIFFFieldWithTag();
extern	int TIFFFlushData1();
extern	int TIFFSetCompressionScheme();
extern	int _TIFFgetfield();
extern	int TIFFNoRowEncode();
extern	int TIFFNoStripEncode();
extern	int TIFFNoTileEncode();
extern	int TIFFNoRowDecode();
extern	int TIFFNoStripDecode();
extern	int TIFFNoTileDecode();
extern	int TIFFNoDecode();
#endif
#endif /* _TIFFIOP_ */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tiff.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* $Header: /usr/people/sam/tiff/libtiff/RCS/tiff.h,v 1.36 92/02/19 09:52:30 sam Exp $ */

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

#ifndef _TIFF_
#define	_TIFF_
/*
 * Tag Image File Format (TIFF)
 *
 * Based on Rev 6.0 from:
 *    Developer's Desk
 *    Aldus Corporation
 *    411 First Ave. South
 *    Suite 200
 *    Seattle, WA  98104
 *    206-622-5500
 */
 
/****** Hardwire-in the support defines ****/

#define PACKBITS_SUPPORT
#define LZW_SUPPORT
#define COLORIMETRY_SUPPORT
#define YCBCR_SUPPORT
#define CARTOGRAPHIC_SUPPORT

/****** end support defines ****************/

#define	TIFF_VERSION	42

#define	TIFF_BIGENDIAN		0x4d4d
#define	TIFF_LITTLEENDIAN	0x4949

/* Needs to be a 4-byte value, but sizeof(u_long) is 8 on 64-bit machines. */
typedef unsigned int tiff_u_long;

typedef	struct {
	unsigned short tiff_magic;	/* magic number (defines byte order) */
	unsigned short tiff_version;	/* TIFF version number */
	tiff_u_long  tiff_diroff;	/* byte offset to first directory */
} TIFFHeader;

/*
 * TIFF Image File Directories are comprised of
 * a table of field descriptors of the form shown
 * below.  The table is sorted in ascending order
 * by tag.  The values associated with each entry
 * are disjoint and may appear anywhere in the file
 * (so long as they are placed on a word boundary).
 *
 * If the value is 4 bytes or less, then it is placed
 * in the offset field to save space.  If the value
 * is less than 4 bytes, it is left-justified in the
 * offset field.
 */
typedef	struct {
	unsigned short tdir_tag;	/* see below */
	unsigned short tdir_type;	/* data type; see below */
	tiff_u_long  tdir_count;	/* number of items; length in spec */
	tiff_u_long  tdir_offset;	/* byte offset to field data */
} TIFFDirEntry;

/*
 * NB: In the comments below,
 *  - items marked with a + are obsoleted by revision 5.0,
 *  - items marked with a ! are introduced in revision 6.0.
 *  - items marked with a $ are obsoleted by revision 6.0.
 */

/*
 * Tag data type information.
 *
 * Note: RATIONALs are the ratio of two 32-bit integer values.
 */
typedef	enum {
	TIFF_NOTYPE	= 0,	/* placeholder */
	TIFF_BYTE	= 1,	/* 8-bit unsigned integer */
	TIFF_ASCII	= 2,	/* 8-bit bytes w/ last byte null */
	TIFF_SHORT	= 3,	/* 16-bit unsigned integer */
	TIFF_LONG	= 4,	/* 32-bit unsigned integer */
	TIFF_RATIONAL	= 5,	/* 64-bit unsigned fraction */
	TIFF_SBYTE	= 6,	/* !8-bit signed integer */
	TIFF_UNDEFINED	= 7,	/* !8-bit untyped data */
	TIFF_SSHORT	= 8,	/* !16-bit signed integer */
	TIFF_SLONG	= 9,	/* !32-bit signed integer */
	TIFF_SRATIONAL	= 10,	/* !64-bit signed fraction */
	TIFF_FLOAT	= 11,	/* !32-bit IEEE floating point */
	TIFF_DOUBLE	= 12	/* !64-bit IEEE floating point */
} TIFFDataType;

/*
 * TIFF Tag Definitions.
 */
#define	TIFFTAG_SUBFILETYPE		254	/* subfile data descriptor */
#define	    FILETYPE_REDUCEDIMAGE	0x1	/* reduced resolution version */
#define	    FILETYPE_PAGE		0x2	/* one page of many */
#define	    FILETYPE_MASK		0x4	/* transparency mask */
#define	TIFFTAG_OSUBFILETYPE		255	/* +kind of data in subfile */
#define	    OFILETYPE_IMAGE		1	/* full resolution image data */
#define	    OFILETYPE_REDUCEDIMAGE	2	/* reduced size image data */
#define	    OFILETYPE_PAGE		3	/* one page of many */
#define	TIFFTAG_IMAGEWIDTH		256	/* image width in pixels */
#define	TIFFTAG_IMAGELENGTH		257	/* image height in pixels */
#define	TIFFTAG_BITSPERSAMPLE		258	/* bits per channel (sample) */
#define	TIFFTAG_COMPRESSION		259	/* data compression technique */
#define	    COMPRESSION_NONE		1	/* dump mode */
#define	    COMPRESSION_CCITTRLE	2	/* CCITT modified Huffman RLE */
#define	    COMPRESSION_CCITTFAX3	3	/* CCITT Group 3 fax encoding */
#define	    COMPRESSION_CCITTFAX4	4	/* CCITT Group 4 fax encoding */
#define	    COMPRESSION_LZW		5	/* Lempel-Ziv  & Welch */
#define	    COMPRESSION_JPEG		6	/* !JPEG compression */
#define	    COMPRESSION_NEXT		32766	/* NeXT 2-bit RLE */
#define	    COMPRESSION_CCITTRLEW	32771	/* #1 w/ word alignment */
#define	    COMPRESSION_PACKBITS	32773	/* Macintosh RLE */
#define	    COMPRESSION_THUNDERSCAN	32809	/* ThunderScan RLE */
#define	TIFFTAG_PHOTOMETRIC		262	/* photometric interpretation */
#define	    PHOTOMETRIC_MINISWHITE	0	/* min value is white */
#define	    PHOTOMETRIC_MINISBLACK	1	/* min value is black */
#define	    PHOTOMETRIC_RGB		2	/* RGB color model */
#define	    PHOTOMETRIC_PALETTE		3	/* color map indexed */
#define	    PHOTOMETRIC_MASK		4	/* $holdout mask */
#define	    PHOTOMETRIC_SEPARATED	5	/* !color separations */
#define	    PHOTOMETRIC_YCBCR		6	/* !CCIR 601 */
#define	    PHOTOMETRIC_CIELAB		8	/* !1976 CIE L*a*b* */
#define	TIFFTAG_THRESHHOLDING		263	/* +thresholding used on data */
#define	    THRESHHOLD_BILEVEL		1	/* b&w art scan */
#define	    THRESHHOLD_HALFTONE		2	/* or dithered scan */
#define	    THRESHHOLD_ERRORDIFFUSE	3	/* usually floyd-steinberg */
#define	TIFFTAG_CELLWIDTH		264	/* +dithering matrix width */
#define	TIFFTAG_CELLLENGTH		265	/* +dithering matrix height */
#define	TIFFTAG_FILLORDER		266	/* data order within a byte */
#define	    FILLORDER_MSB2LSB		1	/* most significant -> least */
#define	    FILLORDER_LSB2MSB		2	/* least significant -> most */
#define	TIFFTAG_DOCUMENTNAME		269	/* name of doc. image is from */
#define	TIFFTAG_IMAGEDESCRIPTION	270	/* info about image */
#define	TIFFTAG_MAKE			271	/* scanner manufacturer name */
#define	TIFFTAG_MODEL			272	/* scanner model name/number */
#define	TIFFTAG_STRIPOFFSETS		273	/* offsets to data strips */
#define	TIFFTAG_ORIENTATION		274	/* +image orientation */
#define	    ORIENTATION_TOPLEFT		1	/* row 0 top, col 0 lhs */
#define	    ORIENTATION_TOPRIGHT	2	/* row 0 top, col 0 rhs */
#define	    ORIENTATION_BOTRIGHT	3	/* row 0 bottom, col 0 rhs */
#define	    ORIENTATION_BOTLEFT		4	/* row 0 bottom, col 0 lhs */
#define	    ORIENTATION_LEFTTOP		5	/* row 0 lhs, col 0 top */
#define	    ORIENTATION_RIGHTTOP	6	/* row 0 rhs, col 0 top */
#define	    ORIENTATION_RIGHTBOT	7	/* row 0 rhs, col 0 bottom */
#define	    ORIENTATION_LEFTBOT		8	/* row 0 lhs, col 0 bottom */
#define	TIFFTAG_SAMPLESPERPIXEL		277	/* samples per pixel */
#define	TIFFTAG_ROWSPERSTRIP		278	/* rows per strip of data */
#define	TIFFTAG_STRIPBYTECOUNTS		279	/* bytes counts for strips */
#define	TIFFTAG_MINSAMPLEVALUE		280	/* +minimum sample value */
#define	TIFFTAG_MAXSAMPLEVALUE		281	/* +maximum sample value */
#define	TIFFTAG_XRESOLUTION		282	/* pixels/resolution in x */
#define	TIFFTAG_YRESOLUTION		283	/* pixels/resolution in y */
#define	TIFFTAG_PLANARCONFIG		284	/* storage organization */
#define	    PLANARCONFIG_CONTIG		1	/* single image plane */
#define	    PLANARCONFIG_SEPARATE	2	/* separate planes of data */
#define	TIFFTAG_PAGENAME		285	/* page name image is from */
#define	TIFFTAG_XPOSITION		286	/* x page offset of image lhs */
#define	TIFFTAG_YPOSITION		287	/* y page offset of image lhs */
#define	TIFFTAG_FREEOFFSETS		288	/* +byte offset to free block */
#define	TIFFTAG_FREEBYTECOUNTS		289	/* +sizes of free blocks */
#define	TIFFTAG_GRAYRESPONSEUNIT	290	/* $gray scale curve accuracy */
#define	    GRAYRESPONSEUNIT_10S	1	/* tenths of a unit */
#define	    GRAYRESPONSEUNIT_100S	2	/* hundredths of a unit */
#define	    GRAYRESPONSEUNIT_1000S	3	/* thousandths of a unit */
#define	    GRAYRESPONSEUNIT_10000S	4	/* ten-thousandths of a unit */
#define	    GRAYRESPONSEUNIT_100000S	5	/* hundred-thousandths */
#define	TIFFTAG_GRAYRESPONSECURVE	291	/* $gray scale response curve */
#define	TIFFTAG_GROUP3OPTIONS		292	/* 32 flag bits */
#define	    GROUP3OPT_2DENCODING	0x1	/* 2-dimensional coding */
#define	    GROUP3OPT_UNCOMPRESSED	0x2	/* data not compressed */
#define	    GROUP3OPT_FILLBITS		0x4	/* fill to byte boundary */
#define	TIFFTAG_GROUP4OPTIONS		293	/* 32 flag bits */
#define	    GROUP4OPT_UNCOMPRESSED	0x2	/* data not compressed */
#define	TIFFTAG_RESOLUTIONUNIT		296	/* units of resolutions */
#define	    RESUNIT_NONE		1	/* no meaningful units */
#define	    RESUNIT_INCH		2	/* english */
#define	    RESUNIT_CENTIMETER		3	/* metric */
#define	TIFFTAG_PAGENUMBER		297	/* page numbers of multi-page */
#define	TIFFTAG_COLORRESPONSEUNIT	300	/* $color curve accuracy */
#define	    COLORRESPONSEUNIT_10S	1	/* tenths of a unit */
#define	    COLORRESPONSEUNIT_100S	2	/* hundredths of a unit */
#define	    COLORRESPONSEUNIT_1000S	3	/* thousandths of a unit */
#define	    COLORRESPONSEUNIT_10000S	4	/* ten-thousandths of a unit */
#define	    COLORRESPONSEUNIT_100000S	5	/* hundred-thousandths */
#define	TIFFTAG_TRANSFERFUNCTION	301	/* !colorimetry info */
#define	TIFFTAG_SOFTWARE		305	/* name & release */
#define	TIFFTAG_DATETIME		306	/* creation date and time */
#define	TIFFTAG_ARTIST			315	/* creator of image */
#define	TIFFTAG_HOSTCOMPUTER		316	/* machine where created */
#define	TIFFTAG_PREDICTOR		317	/* prediction scheme w/ LZW */
#define	TIFFTAG_WHITEPOINT		318	/* image white point */
#define	TIFFTAG_PRIMARYCHROMATICITIES	319	/* !primary chromaticities */
#define	TIFFTAG_COLORMAP		320	/* RGB map for pallette image */
#define	TIFFTAG_HALFTONEHINTS		321	/* !highlight+shadow info */
#define	TIFFTAG_TILEWIDTH		322	/* !rows/data tile */
#define	TIFFTAG_TILELENGTH		323	/* !cols/data tile */
#define TIFFTAG_TILEOFFSETS		324	/* !offsets to data tiles */
#define TIFFTAG_TILEBYTECOUNTS		325	/* !byte counts for tiles */
#define	TIFFTAG_BADFAXLINES		326	/* lines w/ wrong pixel count */
#define	TIFFTAG_CLEANFAXDATA		327	/* regenerated line info */
#define	    CLEANFAXDATA_CLEAN		0	/* no errors detected */
#define	    CLEANFAXDATA_REGENERATED	1	/* receiver regenerated lines */
#define	    CLEANFAXDATA_UNCLEAN	2	/* uncorrected errors exist */
#define	TIFFTAG_CONSECUTIVEBADFAXLINES	328	/* max consecutive bad lines */
#define	TIFFTAG_INKSET			332	/* !inks in separated image */
#define	    INKSET_CMYK			1	/* !cyan-magenta-yellow-black */
#define	TIFFTAG_INKNAMES		333	/* !ascii names of inks */
#define	TIFFTAG_DOTRANGE		336	/* !0% and 100% dot codes */
#define	TIFFTAG_TARGETPRINTER		337	/* !separation target */
#define	TIFFTAG_EXTRASAMPLES		338	/* !info about extra samples */
#define	    EXTRASAMPLE_UNSPECIFIED	0	/* !unspecified data */
#define	    EXTRASAMPLE_ASSOCALPHA	1	/* !associated alpha data */
#define	    EXTRASAMPLE_UNASSALPHA	2	/* !unassociated alpha data */
#define	TIFFTAG_SAMPLEFORMAT		339	/* !data sample format */
#define	    SAMPLEFORMAT_INT		1	/* !signed integer data */
#define	    SAMPLEFORMAT_UINT		2	/* !unsigned integer data */
#define	    SAMPLEFORMAT_IEEEFP		3	/* !IEEE floating point data */
#define	    SAMPLEFORMAT_VOID		4	/* !untyped data */
#define	TIFFTAG_SMINSAMPLEVALUE		340	/* !variable MinSampleValue */
#define	TIFFTAG_SMAXSAMPLEVALUE		341	/* !variable MaxSampleValue */
#define	TIFFTAG_JPEGPROC		512	/* !JPEG processing algorithm */
#define	    JPEGPROC_BASELINE		1	/* !baseline sequential */
#define	    JPEGPROC_LOSSLESS		14	/* !Huffman coded lossless */
#define	TIFFTAG_JPEGIFOFFSET		513	/* !pointer to SOI marker */
#define	TIFFTAG_JPEGIFBYTECOUNT		514	/* !JFIF stream length */
#define	TIFFTAG_JPEGRESTARTINTERVAL	515	/* !restart interval length */
#define	TIFFTAG_JPEGLOSSLESSPREDICTORS	517	/* !lossless proc predictor */
#define	TIFFTAG_JPEGPOINTTRANSFORM	518	/* !lossless point transform */
#define	TIFFTAG_JPEGQTABLES		519	/* !Q matrice offsets */
#define	TIFFTAG_JPEGDCTABLES		520	/* !DCT table offsets */
#define	TIFFTAG_JPEGACTABLES		521	/* !AC coefficient offsets */
#define	TIFFTAG_YCBCRCOEFFICIENTS	529	/* !RGB -> YCbCr transform */
#define	TIFFTAG_YCBCRSUBSAMPLING	530	/* !YCbCr subsampling factors */
#define	TIFFTAG_YCBCRPOSITIONING	531	/* !subsample positioning */
#define	    YCBCRPOSITION_CENTERED	1	/* !as in PostScript Level 2 */
#define	    YCBCRPOSITION_COSITED	2	/* !as in CCIR 601-1 */
#define	TIFFTAG_REFERENCEBLACKWHITE	532	/* !colorimetry info */
/* tags 32995-32999 are private tags registered to SGI */
#define	TIFFTAG_MATTEING		32995	/* $use ExtraSamples */
#define	TIFFTAG_DATATYPE		32996	/* $use SampleFormat */
#define	TIFFTAG_IMAGEDEPTH		32997	/* z depth of image */
#define	TIFFTAG_TILEDEPTH		32998	/* z depth/data tile */
#ifdef CARTOGRAPHIC_SUPPORT
/*
	Reserved Tags for Cartographic Information (JPL Carto Lab):
	The tag values from 34263 to 34267 inclusive are reserved for
	use by JPL Carto Group, registered with Aldus Develpmt.
	(via Andrew Commons at Aldus).
	
	Since we only have 5, the first tag will be an offset to a
	private TIFF Image File Header (IFD) into which we may put
	any of our own tag values (2**16 values available). The only
	tag a standard reader will see is the first value (unless a
	future proposal adds carto-like tags to the standard).
*/
#define CARTB	34263					 	/* Base of REGISTERED Cartographic Tags */
#define TIFFTAG_CARTO_IFD_OFFSET CARTB      /* Offset in file to Carto IFD LONG*/
#define TIFFCARTLAST 34267					/* END of REGISTERED Cartographic Tags */
/*
 * Though these next tag values will not be seen in a standard TIFF IFD, we
 * will start them at our tag values and call them CARTTAG_xxx, indicating
 * that these are private IFD values, not registered.
 */
#define	CARTTAG_PROJECTIONTYPE	CARTB+1	/* Type of Map Projection SHORT*/
#define	    PROJECTIONTYPE_UTM	0			/* Universal Transverse Mercator */
#define	CARTTAG_PROJ_XPOS		CARTB+2	/* Horiz. Pixel loc. of ref. point LONG*/
#define	CARTTAG_PROJ_YPOS		CARTB+3	/* Verti. Pixel loc. of ref. point LONG*/
#define	CARTTAG_LATITUDE		CARTB+4	/* Latitude (angle units) ref. point LONG*/
#define	CARTTAG_LONGITUDE		CARTB+5	/* Longitude (angle units) ref. point LONG*/
#define	CARTTAG_XPIXPERANGLE	CARTB+6	/* Horiz Pixels per angular unit RATIONAL*/
#define	CARTTAG_YPIXPERANGLE	CARTB+7	/* Verti Pixels per angular unit RATIONAL*/
#endif /* CARTO */

#endif /* _TIFF_ */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tiffio.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* $Header: /usr/people/sam/tiff/libtiff/RCS/tiffio.h,v 1.53 92/02/19 14:24:20 sam Exp $ */

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
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

#ifndef _TIFFIO_
#define	_TIFFIO_

/*
 * TIFF I/O Library Definitions.
 */
#include "tiff.h"

/*
 * TIFF is defined as an incomplete type to hide the
 * library's internal data structures from clients.
 */
typedef	struct tiff TIFF;

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL (void*) 0
#endif
#endif

/*
 * Flags to pass to TIFFPrintDirectory to control
 * printing of data structures that are potentially
 * very large.   Bit-or these flags to enable printing
 * multiple items.
 */
#define	TIFFPRINT_NONE		0x0		/* no extra info */
#define	TIFFPRINT_STRIPS	0x1		/* strips/tiles info */
#define	TIFFPRINT_CURVES	0x2		/* color/gray response curves */
#define	TIFFPRINT_COLORMAP	0x4		/* colormap */
#define	TIFFPRINT_JPEGQTABLES	0x100		/* JPEG Q matrices */
#define	TIFFPRINT_JPEGACTABLES	0x200		/* JPEG AC tables */
#define	TIFFPRINT_JPEGDCTABLES	0x200		/* JPEG DC tables */

#if defined(__STDC__) || defined(__EXTENDED__) || USE_CONST
extern const char TIFFVersion[];
extern const unsigned char TIFFBitRevTable[256];
extern const unsigned char TIFFNoBitRevTable[256];
#else
extern char TIFFVersion[];
extern unsigned char TIFFBitRevTable[256];
extern unsigned char TIFFNoBitRevTable[256];
#endif

/*
 * Macros for extracting components from the
 * packed ABGR form returned by TIFFReadRGBAImage.
 */
#define	TIFFGetR(abgr)	((abgr) & 0xff)
#define	TIFFGetG(abgr)	(((abgr) >> 8) & 0xff)
#define	TIFFGetB(abgr)	(((abgr) >> 16) & 0xff)
#define	TIFFGetA(abgr)	(((abgr) >> 24) & 0xff)

#if defined(c_plusplus) || defined(__cplusplus) || defined(__STDC__) || defined(__EXTENDED__) || USE_PROTOTYPES
#include <stdio.h>
#include <stdarg.h>

typedef	void (*TIFFErrorHandler)(char* module, char* fmt, va_list ap);
typedef	void (*TIFFPrintHandler)(FILE* fd, char* fmt, va_list ap);

#if defined(__cplusplus)
extern "C" {
#endif
extern	void TIFFClose(TIFF*);
extern	int TIFFFlush(TIFF*);
extern	int TIFFFlushData(TIFF*);
extern	int TIFFGetField(TIFF*, int, ...);
extern	int TIFFVGetField(TIFF*, int, va_list);
extern	int TIFFGetFieldDefaulted(TIFF*, int, ...);
extern	int TIFFVGetFieldDefaulted(TIFF*, int, va_list);
extern	int TIFFReadDirectory(TIFF*);
extern  tiff_u_long TIFFScanlineSize(TIFF*);
extern	tiff_u_long TIFFStripSize(TIFF*);
extern	tiff_u_long TIFFVStripSize(TIFF*, tiff_u_long);
extern	tiff_u_long TIFFTileRowSize(TIFF*);
extern	tiff_u_long TIFFTileSize(TIFF*);
extern	tiff_u_long TIFFVTileSize(TIFF*, tiff_u_long);
extern	int TIFFFileno(TIFF*);
extern	int TIFFGetMode(TIFF*);
extern	int TIFFIsTiled(TIFF*);
extern	int TIFFhasCartoTags(TIFF*);
extern	long TIFFCurrentRow(TIFF*);
extern	int TIFFCurrentDirectory(TIFF*);
extern	int TIFFCurrentStrip(TIFF*);
extern	int TIFFCurrentTile(TIFF*);
extern	int TIFFReadBufferSetup(TIFF*, char*, tiff_u_long);
extern	int TIFFSetDirectory(TIFF*, int);
extern	int TIFFSetField(TIFF*, int, ...);
extern	int TIFFVSetField(TIFF*, int, va_list);
extern	int TIFFWriteDirectory(TIFF *);
#if defined(c_plusplus) || defined(__cplusplus)
extern	TIFF* TIFFOpen(const char*, const char*);
extern	TIFF* TIFFFdOpen(const int, const char*, const char*);
extern	const char* TIFFFileName(TIFF*);
extern	void TIFFPrint(FILE*, const char*, ...);
extern	void TIFFError(const char*, const char*, ...);
extern	void TIFFWarning(const char*, const char*, ...);
extern	void TIFFPrintDirectory(TIFF*, FILE*, long = 0);
extern	int TIFFReadScanline(TIFF*, unsigned char*, unsigned, unsigned = 0);
extern	int TIFFWriteScanline(TIFF*, unsigned char*, unsigned, unsigned = 0);
extern	int TIFFReadRGBAImage(TIFF*, tiff_u_long, tiff_u_long, tiff_u_long*, int stop = 0);
#else
extern	TIFF* TIFFOpen(char*, char*);
extern	TIFF* TIFFFdOpen(int, char*, char*);
extern	char* TIFFFileName(TIFF*);
extern	void TIFFPrint(FILE*, char*, ...);
extern	void TIFFError(char*, char*, ...);
extern	TIFFErrorHandler TIFFSetErrorHandler(TIFFErrorHandler handler);
extern	TIFFPrintHandler TIFFSetPrintHandler(TIFFPrintHandler handler);
extern	void TIFFWarning(char*, char*, ...);
extern	TIFFErrorHandler TIFFSetWarningHandler(TIFFErrorHandler handler);
extern	void TIFFPrintDirectory(TIFF*, FILE*, long);
extern	int TIFFReadScanline(TIFF*, unsigned char*, unsigned, unsigned);
extern	int TIFFWriteScanline(TIFF*, unsigned char*, unsigned, unsigned);
extern	int TIFFReadRGBAImage(TIFF*, tiff_u_long, tiff_u_long, tiff_u_long*, int stop);
#endif
extern	unsigned int TIFFComputeTile(TIFF*, tiff_u_long, tiff_u_long, tiff_u_long, unsigned int);
extern	int TIFFCheckTile(TIFF*, tiff_u_long, tiff_u_long, tiff_u_long, unsigned);
extern	unsigned int TIFFNumberOfTiles(TIFF*);
extern	int TIFFReadTile(TIFF*, unsigned char*, tiff_u_long, tiff_u_long, tiff_u_long, unsigned);
extern	int TIFFWriteTile(TIFF *, unsigned char*, tiff_u_long, tiff_u_long, tiff_u_long, unsigned int);
extern	unsigned int TIFFComputeStrip(TIFF*, tiff_u_long, unsigned int);
extern	unsigned int TIFFNumberOfStrips(TIFF*);
extern	long TIFFReadEncodedStrip(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	long TIFFReadRawStrip(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	long TIFFReadEncodedTile(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	long TIFFReadRawTile(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	long TIFFWriteEncodedStrip(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	long TIFFWriteRawStrip(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	long TIFFWriteEncodedTile(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	long TIFFWriteRawTile(TIFF*, unsigned, unsigned char*, tiff_u_long);
extern	int TIFFSwabShort(unsigned short *);
extern	int TIFFSwabLong(tiff_u_long *);
extern	int TIFFSwabArrayOfShort(unsigned short *, int);
extern	int TIFFSwabArrayOfLong(tiff_u_long *, int);
extern	int TIFFReverseBits(unsigned char *, int);
#if defined(__cplusplus)
}
#endif
#else
typedef	void (*TIFFErrorHandler)();
typedef	void (*TIFFPrintHandler)();

extern	void TIFFClose();
extern	TIFF *TIFFOpen();
extern	TIFF *TIFFFdOpen();
extern	char* TIFFFileName();
extern	int TIFFFileno();
extern	int TIFFGetMode();
extern	int TIFFIsTiled();
extern	int TIFFhasCartoTags();
extern	unsigned int TIFFComputeTile();
extern	long TIFFCurrentRow();
extern	int TIFFCurrentDirectory();
extern	int TIFFCurrentStrip();
extern	int TIFFCurrentTile();
extern	void TIFFPrint();
extern	void TIFFError();
extern	TIFFErrorHandler TIFFSetErrorHandler();
extern	TIFFPrintHandler TIFFSetPrintHandler();
extern	int TIFFFlush();
extern	int TIFFFlushData();
extern	int TIFFGetField();
extern	int TIFFVGetField();
extern	int TIFFGetFieldDefaulted();
extern	int TIFFVGetFieldDefaulted();
extern	unsigned int TIFFNumberOfTiles();
extern	void TIFFPrintDirectory();
extern	int TIFFReadDirectory();
extern	int TIFFReadBufferSetup();
extern	int TIFFReadScanline();
extern	int TIFFReadTile();
extern	int TIFFWriteTile();
extern	unsigned int TIFFComputeStrip();
extern	unsigned int TIFFNumberOfStrips();
extern  long TIFFReadEncodedStrip();
extern  long TIFFReadRawStrip();
extern	long TIFFReadEncodedTile();
extern	int TIFFReadRGBAImage();
extern	long TIFFReadRawTile();
extern	tiff_u_long TIFFScanlineSize();
extern	tiff_u_long TIFFStripSize();
extern	tiff_u_long TIFFVStripSize();
extern	tiff_u_long TIFFTileRowSize();
extern	tiff_u_long TIFFTileSize();
extern	tiff_u_long TIFFVTileSize();
extern	int TIFFSetDirectory();
extern	int TIFFSetField();
extern	int TIFFVSetField();
extern	void TIFFWarning();
extern	TIFFErrorHandler TIFFSetWarningHandler();
extern	int TIFFWriteDirectory();
extern	int TIFFWriteScanline();
extern	long TIFFWriteEncodedStrip();
extern	long TIFFWriteRawStrip();
extern	long TIFFWriteEncodedTile();
extern	long TIFFWriteRawTile();
extern	int TIFFSwabShort();
extern	int TIFFSwabLong();
extern	int TIFFSwabArrayOfShort();
extern	int TIFFSwabArrayOfLong();
extern	int TIFFReverseBits();
extern	int TIFFCheckTile();
#endif
#endif /* _TIFFIO_ */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vtiff.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/** Source code for:      vtiff.c
** The library of TIFF file routines used in vtiff is based on a public-
** domain suite of software, with additional enhancements for JPL use.
** The original source contains the following copyright notice:
**
** Tag Image File Format Library
**
** Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
** Copyright (c) 1991, 1992 Silicon Graphics, Inc.
** 
** Permission to use, copy, modify, distribute, and sell this software and 
** its documentation for any purpose is hereby granted without fee, provided
** that (i) the above copyright notices and this permission notice appear in
** all copies of the software and related documentation, and (ii) the names
** of Sam Leffler and Silicon Graphics may not be used in any advertising or
** publicity relating to the software without the specific, prior written
** permission of Stanford and Silicon Graphics.
** 
** THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
** EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
** WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
** 
** IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
** ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
** OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
** WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
** LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
** OF THIS SOFTWARE.
**
** Library-wide configuration defines:
**    MMAP_SUPPORT	add support for memory mapping read-only files
**    COLORIMETRY_SUPPORT add support for 6.0 colorimetry tags
**    JPEG_SUPPORT	add support for 6.0 JPEG tags & JPEG algorithms
**    YCBCR_SUPPORT	add support for 6.0 YCbCr tags
**    CMYK_SUPPORT	add support for 6.0 CMYK tags
**
** Compression configuration defines:
**    CCITT_SUPPORT	add support for CCITT Group 3 & 4 algorithms
**    PACKBITS_SUPPORT	add support for Macintosh PackBits algorithm
**    LZW_SUPPORT	add support for LZW algorithm
**    THUNDER_SUPPORT	add support for ThunderScan 4-bit RLE algorithm
**    NEXT_SUPPORT	add support for NeXT 2-bit RLE algorithm
**    JPEG_SUPPORT	add support for JPEG DCT algorithm
**
**/

/* 03oct2011 -lwk- changed some of the variables returned by TIFFGetField
		from int to tiff_u_long as that's what it expects (they were
		overrunning their buffer) */

#include "tiffio.h"
#include "vicmain_c"
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef MIN
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#endif

void tiff_to_vic(void);
void vic_to_tiff(void);
void WriteTIFFClut(TIFF *tif);
void WriteFileFromTIFF(char *parm,int instance,TIFF *tif);
void SetUpVicarFile(char *parm,int instance,TIFF *in,int outunit[],
			int *nl,int *ns,int *nb);
int WriteVicarData(int unit[3],TIFF *tif,int nl,int ns,int nb);

void main44(void)
{   
    int count,def;
    char command[12];
    
    zvparm("_SUBCMD",command,&count,&def,0,0);
    switch (toupper(command[0]))
    {
      case 'F' : vic_to_tiff();
               break;
      case 'T' : tiff_to_vic();
               break;
    }
}

static int first=1;

PrintHandler(char *fd,char *fmt,va_list ap)
{
	static char message[132];
	char txt[132];
	int len;

	if (first)
	{
	   first=0;
           message[0]='\0';
	}

        vsprintf(txt, fmt, ap);
	strcat(message,txt);
	len=strlen(message);
	if (message[len-1]=='\n')
	{
	  message[len-1]='\0'; 
          zvmessage(message, " ");
	  message[0]='\0';
        }
}

AbortMsg(message)
char *message;
{
      zvmessage(message,"");
      zabend();
}

/*********************************************************************/
/*********************** tiff_to_vic        **************************/
/*********************************************************************/

void tiff_to_vic(void)
{
      int i,count;
      TIFF  *in;
      char inname[133];

/* open input TIFF file */
      zvselpi(0);
      zvpone("INP", inname, 1, 132);
      in = TIFFOpen(inname, "r");
      if (in == NULL) AbortMsg("Error opening TIFF file");

      if (zvptst("DUMP"))
      {
        TIFFSetPrintHandler( (TIFFPrintHandler)PrintHandler );
	TIFFPrintDirectory(in,0L,TIFFPRINT_NONE);
      }

      GetTIFFGeoreference(in);

/* If desired, convert to lookup table and write out to file */
      zvpcnt("OUT",&count);
      if (count==2) WriteTIFFClut(in);

/* convert TIFF to output file */
      zveaction("SA", "");
      WriteFileFromTIFF("OUT",1,in);      


/* add auxilliary files, if any */
      zvpcnt("AUXIL",&count);
      for (i=0; i<count; i++)
      {
            if (!TIFFReadDirectory(in))
                  AbortMsg("Not enough images in TIFF file for auxilliaries");
            WriteFileFromTIFF("AUXIL",i+1,in);
      }
      
/* close up shop */
      TIFFClose(in);


}

void WriteFileFromTIFF(char *parm,int instance,TIFF *tif)
{
      int  unit[3],nl,ns,nb,i;

      SetUpVicarFile(parm,instance,tif,unit,&nl,&ns,&nb);
      if (!WriteVicarData(unit,tif,nl,ns,nb))
            AbortMsg("Error in Copying data to TIFF ");

      for (i=0; i<nb; i++)
	      zvclose(unit[i], NULL);
}


void SetUpVicarFile(char *parm,int instance,TIFF *in,int outunit[],
			int *nl,int *ns,int *nb)
{
      int i;
      uint16_t nbs;
      char name[132];
      int numout;
      long tifinfo;

      zvpcnt("OUT", &numout);
     
/* get info from TIFF file */
      TIFFGetField(in,TIFFTAG_IMAGEWIDTH,&tifinfo);
      *ns = tifinfo;
      TIFFGetField(in,TIFFTAG_IMAGELENGTH,&tifinfo);
      *nl = tifinfo;
      if (!TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &nbs))
        nbs=1;
      *nb = nbs;
      if (*nb < 1) *nb=1;
      if (*nb > 3) *nb=3;
      if (*nb==3 && numout<3)
      {
	   zvmessage("Cannot compress RGB TIFF to 8-bit"," ");
	   zabend();
      }

      zvselpi(0);  /* INP is not a VICAR file */

      for (i=0;i<*nb;i++)
      {
	      zvpone(parm, name, instance+i, 132);
	      zvunit(outunit+i, "xxx", instance+i,"U_NAME",name, NULL);
	      
		/* Open up VICAR file with proper size */
	
	      zvopen(outunit[i],"OP","WRITE","U_NL",*nl,"U_NS",*ns, NULL);
	      
		/* Reopen in UPDATE for random access */
	      zvclose(outunit[i], NULL);
	      zvopen(outunit[i],"OP","UPDATE", NULL);
      }
}


int WriteVicarData(int unit[3],TIFF *tif,int nl,int ns,int nb)
{
      uint8_t *buf,*lbuf[3],*bptr[3],*ptr;
      uint32_t x,y,y1,y2,z;
      uint16_t planar;
      int nsamps;
      int tilebufsize;
      int tilepix;
      tiff_u_long tile_width,tile_height;
      int scanbytes;
      int band;
      int nrows;
      int chunky;
      int i;
      int plane_inc;
      int tile,tile1;
      long (*_tiff_read)();

      TIFFGetField( tif, TIFFTAG_PLANARCONFIG, &planar);
      chunky = (planar == PLANARCONFIG_CONTIG);

	
	if (TIFFIsTiled(tif))
	{
		plane_inc = TIFFComputeTile(tif, 0,0,0,1)
				- TIFFComputeTile(tif, 0,0,0,0);
		TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_TILELENGTH, &tile_height);
		tilebufsize = TIFFTileSize(tif);
		_tiff_read = TIFFReadEncodedTile;
	}
	else
	{
		plane_inc = TIFFComputeStrip(tif, 0, 1)
				- TIFFComputeStrip(tif, 0, 0);
		TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &tile_height);
		tilebufsize = TIFFStripSize( tif );
		_tiff_read = TIFFReadEncodedStrip;
	}
	tilepix = tile_width*tile_height;
	buf = (uint8_t *)malloc(tilebufsize);
	if (chunky)
		lbuf[0] = (uint8_t *)malloc(tilebufsize);
	else
		lbuf[0] = (uint8_t *)malloc(tilebufsize*nb);
	for (i=1;i<nb;i++) 
		lbuf[i] = lbuf[0]+ i*tile_width*tile_height;
	if (!buf || !lbuf[0]) 
		AbortMsg("failed to allocate buffer for tile/strip");
	
	tile=0;
	for (y = 0; y < nl; y+=tile_height)
	{
		nrows = MIN(tile_height, nl - y);
		for (x = 0; x < ns; x+=tile_width)
		{
			memset(buf,0,(size_t)tilebufsize);
			if (chunky)
			{
				if (_tiff_read(tif, tile++, buf, tilebufsize) < 0)
					goto bad;
				/* reshuffle pixels into chunky: rgbrgb...*/
				ptr=buf;
				for (i=0;i<nb;i++) bptr[i] = lbuf[i];
				for (z=0; z<tilepix; z++)
					for (band=0;band<nb;band++)
						*bptr[band]++ = *ptr++;
			}
			else /* interleave by tile */
			{
				tile1=tile++;
				for (band=0; band<nb; band++)
				{
					if (_tiff_read(tif, tile1, lbuf[band], tilebufsize) < 0)
						goto bad;
					tile1 += plane_inc;
				}
			}

			/* write out a single tile */
			nsamps = MIN(tile_width, ns - x);
			for (y1=0,y2=0; y1< nrows; y1++)
			{
				for (band=0;band<nb;band++)
				{
					zvwrit(unit[band],lbuf[band]+y2,
					"line",y+y1+1, "samp",x+1,"nsamps", nsamps, NULL);
				}
				y2 += tile_width;
			}
		}
	}
      
      free(buf);
      free(lbuf[0]);
      return (1);
bad:
      free(buf);
      free(lbuf[0]);
      return (0);

}


/*********************************************************************/
/*********************** vic_to_tiff        **************************/
/*********************************************************************/


void vic_to_tiff(void)
{
      int i,count;
      TIFF  *out;
      char outname[133];
      char tiffmode[10];
      
/* open output TIFF file */
      zvpone("out", outname, 1, 132);
      out = TIFFOpen(outname, "w");
      if (out == NULL) AbortMsg("Error opening TIFF file");

/* Put application-specific Resource data in description tag: */
      AddTIFFGeoreference(out);

/* convert input file to TIFF */
      zveaction("SA", "");
      zvp("TIFFMODE",tiffmode,&count);
      AddFiletoTIFF("INP",out,tiffmode); 

/* add auxilliary files, if any, using strips */
      zvpcnt("AUXIL",&count);
      if (count > 0)
      {
            TIFFWriteDirectory(out);
            AddFiletoTIFF("AUXIL",out,"strips");
      }
      
/* close up shop */
      TIFFClose(out);
}


GetTIFFGeoreference(in)
TIFF *in;
{
	long Lat,Long;
	long vPix[2];
	long hPix[2];
	long xPos;
	long yPos;
	int count;
	int projType;
	float hres,vres;

	  if (TIFFhasCartoTags(in))
	  {
		  /*
		   * new stuff -- These are "CARTTAG" because they
		   * are placed only in a private TIFF IFD and are not
		   * registered (though the file offset to IFD is).
		   */

		  /* Should be PROJECTIONTYPE_UTM, :*/
		  TIFFGetField(in, CARTTAG_PROJECTIONTYPE,&projType);
		  TIFFGetField(in, CARTTAG_PROJ_XPOS,&xPos);
		  TIFFGetField(in, CARTTAG_PROJ_YPOS,&yPos);
		  TIFFGetField(in, CARTTAG_LATITUDE,&Lat);
		  TIFFGetField(in, CARTTAG_LONGITUDE,&Long);
		  TIFFGetField(in, CARTTAG_XPIXPERANGLE,&hres);
		  TIFFGetField(in, CARTTAG_YPIXPERANGLE,&vres);
		  hres *= 36e5;
		  vres *= 36e5;
		  hPix[0] = hres;
		  hPix[1] = (hres - (double)hPix[0]) * (uint32_t)0xFFFFFFFF;
		  vPix[0] = vres;
		  vPix[1] = (vres - (double)vPix[0]) * (uint32_t)0xFFFFFFFF;
	  }

	/** should write out to PROPERTY label **/

}

/* convert deg-min-sec to thousanths of second */

double DMSToSec1000(angl,count)
float angl[3];
int count;
{
	register double s1000=0.0;
	enum {deg,min,sec};
	
	switch (count-1)
	{
		case sec:
			s1000+=angl[sec]*1e3;  /* fall through */
		case min:
			s1000+=angl[min]*6e4;  /* fall through */
		case deg:
			s1000+=angl[deg]*36e5;
			break;
		default: /* default value is 1.0 */
			s1000=1.0;
			break;
	}
	
	return s1000;
}

#define EQUATOR 90L*60*60*1000
#define PRIME_MERIDIAN 180L*60*60*1000

AddTIFFGeoreference(out)
TIFF *out;
{
	float theLat[3],theLong[3];
	float hDegPix[3],vDegPix[3];
	uint32_t lat1000sec,long1000sec;
	uint32_t vPix[2];
	uint32_t hPix[2];
	uint32_t xPos,yPos;
	char cart_string[500];
	int def,count,location_set;
	double hres,vres;

	/** look for PROPERTY label, else get from parms **/
	
	  zvparm("LAT",theLat,&count,&def,0,0);
	  location_set = (!def);
	  if (location_set)
	  {
		  lat1000sec=DMSToSec1000(theLat,count);
		  if (zvptst("NORTH"))
			lat1000sec=EQUATOR+lat1000sec;
		  else
			lat1000sec=EQUATOR-lat1000sec;
			
		  zvp("LONG",theLong,&count);
		  long1000sec=DMSToSec1000(theLong,count);
		  if (zvptst("EAST"))
			long1000sec=PRIME_MERIDIAN+long1000sec;
		  else
			long1000sec=PRIME_MERIDIAN-long1000sec;
			
		  /* hres is in pixels per thousandths of second*/
		  zvp("HDEGPIX",hDegPix,&count);
		  hres = (double) 1.0 / DMSToSec1000(hDegPix,count);
		  zvp("VDEGPIX",vDegPix,&count);
		  vres = (double) 1.0 / DMSToSec1000(vDegPix,count);
	
		  /* hpix are in pixels per degree */
		  hPix[0] = hres*36e5;
		  hPix[1] = (hres*36e5 - (double)hPix[0]) * (uint32_t)0xFFFFFFFF;
		  vPix[0] = vres*36e5;
		  vPix[1] = (vres*36e5 - (double)vPix[0]) * (uint32_t)0xFFFFFFFF;
	
		  zvp("XPIXPOS",&xPos,&count);
		  zvp("YPIXPOS",&yPos,&count);

		  
		  /*
		   * Old method of storing info - for backward compatibility
		   */
		  sprintf(cart_string,
				"CART_RESOURCE(%d,%ld,%ld,%ld,%lu,%ld,%lu,%ld,%ld)",
				 0,long1000sec,lat1000sec,hPix[0],hPix[1],
				   vPix[0],vPix[1],xPos,yPos);
		  TIFFSetField(out, TIFFTAG_IMAGEDESCRIPTION,cart_string);
		  
		  /*
		   * new stuff -- These are "CARTTAG" because they
		   * are placed only in a private TIFF IFD and are not
		   * registered.
		   */
		  TIFFSetField(out, CARTTAG_PROJECTIONTYPE,PROJECTIONTYPE_UTM);
		  TIFFSetField(out, CARTTAG_PROJ_XPOS,xPos);
		  TIFFSetField(out, CARTTAG_PROJ_YPOS,yPos);
		  TIFFSetField(out, CARTTAG_LONGITUDE,long1000sec);
		  TIFFSetField(out, CARTTAG_LATITUDE,lat1000sec);
		  TIFFSetField(out, CARTTAG_XPIXPERANGLE,hres);
		  TIFFSetField(out, CARTTAG_YPIXPERANGLE,vres);
	  }
}

AddFiletoTIFF(parm,tif,mode)
char *parm;
TIFF *tif;
char *mode;      /* "tiled" or "Strips" */
{
      int  unit[3];
      int i,nl,ns,nb,numinp,status;
      int bit8;
      char name[201];

      zvpcnt(parm, &numinp);
      bit8 = zvptst("bit8");
     
   
      for (i=0; i<numinp; i++)
      {
          zvpone(parm, name, i+1, 200);
          zvunit(unit+i, parm, i+1,"U_NAME",name, NULL);
          status = zvopen(unit[i],"OP","READ","U_FORMAT","BYTE", NULL);
      }

      nb = (numinp==3) ? 3 : 1;
      
      if (numinp==3 && bit8)
      {
      		ConvertToLut( unit );
      		nb = 1;
      		numinp=2;
      }
      
      switch (*parm)
      {
	      case 'i':case 'I':
	         TIFFSetField(tif, TIFFTAG_SUBFILETYPE, 0);
	         break;
	      default:
	         TIFFSetField(tif, TIFFTAG_SUBFILETYPE, FILETYPE_REDUCEDIMAGE);
	      break;
      }

      SetUpTiffDirectory(unit,tif,mode,&nl,&ns,nb,numinp);
      if (!WriteTIFFData(unit,tif,nl,ns,nb))
            AbortMsg("Error in Copying data to TIFF ");
	    
      for (i=0; i<nb; i++)
          zvclose(unit[i], NULL);
}

#define MAX_DIM 512

int ConvertToLut( unit )
int unit[];
{
	int tempunit[2];
	int i;
	int line,samp=0;
	int status=1;
	int lineinc,sampinc;
	int nl,ns,numcolors,size;
	unsigned char *lbuffer[3];
	unsigned char *index;
	unsigned char *red,*grn,*blu,*iptr;
	int *table=(int *)0;
	
     	zvget(unit[0],"NL",&nl,"NS",&ns, NULL);
     	lineinc = (nl + MAX_DIM-1)/MAX_DIM;
     	sampinc = (ns + MAX_DIM-1)/MAX_DIM;
	while (!(ns % sampinc)) sampinc++;
	
	/* Create a temporary index and LUT file */
	
	status = zvunit( tempunit+0, "xxx",5,"u_name","vtiff_temp_img", NULL);
	status = zvopen( tempunit[0], "op", "write","u_nl",nl,"u_ns",ns, NULL);

	status = zvunit( tempunit+1, "xxx",6,"u_name","vtiff_temp_lut", NULL);
	status = zvopen( tempunit[1], "op", "write","u_nl",1,"u_ns",1024, NULL);
	
	/* allocate memory */
	
	if (!table_new( &table, 254 ))
	{
		zvmessage("Memory error creating color table"," ");
		zabend();
	}
	size = (ns<256) ? 256 : ns;
	for (i=0;i<3;i++)
	{
		lbuffer[i] = (unsigned char *)malloc(size);
		if (!lbuffer[i])
		{
			zvmessage("Memory error allocating LUT buffer"," ");
			zabend();
		}
	}
	index = (unsigned char *)malloc(size < 1024 ? 1024 : size);
	if (!index) 
	{
		zvmessage("Memory error creating index table"," ");
		zabend();
	}

	
	/* run through image & collect colors */
	
	for (line=1; line<=nl; line+=lineinc)
	{
		for (i=0;i<3;i++)
			zvread(unit[i],lbuffer[i],"line",line, NULL);
		
		red = lbuffer[0]; grn=lbuffer[1]; blu=lbuffer[2];
		numcolors=0;
		for (samp=samp%ns; samp<ns; samp+=sampinc,numcolors++)
		{
			red[numcolors] = red[samp];
			grn[numcolors] = grn[samp];
			blu[numcolors] = blu[samp];
		}
		
	        table_add_colors(table, red, grn, blu, numcolors );

	}
	
	table_build( table );  /* computes optimal color table */
	
	/* write out INDEX image */
	red = lbuffer[0]; grn=lbuffer[1]; blu=lbuffer[2];
	for (line=1; line<=nl; line++)
	{
		for (i=0;i<3;i++)
			zvread(unit[i],lbuffer[i],"line",line, NULL);

      		table_rgb_to_index( table, red, grn, blu, ns, index );
      		zvwrit( tempunit[0], index, NULL);
	}
	
	/* write out LUT */
	table_extract_lut( table, red, grn, blu);
	iptr = index;
	memset(index, 0, 1024);
	for (i=0;i<256;i++)
	{
		*iptr++ = *red++;
		*iptr++ = *grn++;
		*iptr++ = *blu++;
		iptr++;
	}
	zvwrit( tempunit[1], index, NULL);

	/* close old files * free up memory */
	for (i=0;i<3;i++)
	{
		zvclose( unit[i], NULL);
		free(lbuffer[i]);
	}
	free (index);
    	table_free( table );
	
	/* reset units */
	status = zvclose( tempunit[0], NULL);
	status = zvopen( tempunit[0], "op", "read","clos_act", "delete", NULL);
	status = zvclose( tempunit[1], NULL);
	status = zvopen( tempunit[1], "op", "read","clos_act", "delete", NULL);
	unit[0] = tempunit[0];
	unit[1] = tempunit[1];
	
	return status;
}

/* This will probably do for initializing most TIFF files */

SetUpTiffDirectory(inunit,out,mode,nl,ns,nb,numinp)
int inunit[];
TIFF *out;
char *mode;      /* "tiled" or "Strips" */
int *nl;
int *ns;	 /* output: sizes */
int nb;		 /* input : number of bands */
int numinp;	/* number of VICAR units in inunit */
{
      int count,def,tiled,status,i;
      tiff_u_long tile_width,tile_height;
      int resunit;
      float xresolution,yresolution;
      char *tiff_time();
      uint16_t clut[3][256];
      uint16_t compression;

      tiled = (toupper(*mode) == 'T');

      resunit = zvptst( "INCH") ? RESUNIT_INCH : RESUNIT_CENTIMETER;
      zvp( "XRES", &xresolution, &def); 
      zvp( "YRES", &yresolution, &def); 
      TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, resunit);
      TIFFSetField(out, TIFFTAG_XRESOLUTION, (double)xresolution);
      TIFFSetField(out, TIFFTAG_YRESOLUTION, (double)yresolution);

/* Initial tags which are always the same for our applications */
      TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 8);

 
/* Compression */
      if (zvptst("LZW"))
      		compression = COMPRESSION_LZW;
      else if (zvptst("PACKBITS"))
      		compression = COMPRESSION_PACKBITS;
      else               
      		compression = COMPRESSION_NONE;

      TIFFSetField(out, TIFFTAG_COMPRESSION, compression);

/* Tiling parameters */
	zvp("TLINES",&tile_height,&count);
	zvp("TSAMPS",&tile_width,&count);
      if (tiled)
      {
	    if (tile_height%8 || tile_width%8)
	    {
	    	zvmessage("Tile size must be a multiple of 8"," ");
		zabend();
	    }
            TIFFSetField(out, TIFFTAG_TILEWIDTH, tile_width);
            TIFFSetField(out, TIFFTAG_TILELENGTH, tile_height);
      }
	else
	  {
      		TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, tile_height);
	  }      

      
/* Set width,length, and bands (called "samples" in TIFF) */
      zvget(inunit[0],"NL",nl,"NS",ns, NULL);

      TIFFSetField(out, TIFFTAG_IMAGEWIDTH,(uint32_t) *ns);
      TIFFSetField(out, TIFFTAG_IMAGELENGTH,(uint32_t) *nl);


/* Name of the program creating this tiff file: */
      TIFFSetField(out, TIFFTAG_SOFTWARE, "VICAR Program VTIFF");

/* Set the TIFF standard date-time */
      TIFFSetField(out, TIFFTAG_DATETIME, tiff_time());

/* set the color type and color map, if any */
      if (zvptst( "chunky" ))
          TIFFSetField(out, TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
      else
          TIFFSetField(out, TIFFTAG_PLANARCONFIG,PLANARCONFIG_SEPARATE);
	  
      TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, nb);

      if (nb==1)
      {
      	  if (numinp == 2)
	  {
      	      TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
	      SetTIFFClut(clut,inunit[1],"DiskFile");
	      TIFFSetField(out, TIFFTAG_COLORMAP, clut[0],clut[1],clut[2]);
	  }
	  else TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

      }
      else
      {
      	  TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
      }
}

SetTIFFClut(clut,lutunit,type)
unsigned short clut[3][256];
int lutunit;
char *type; /* "Gray" or "Disk" */
{
      uint16_t i,j,val;
      enum {red,green,blue};
      unsigned char idx_lut[256][4];
      
      switch (toupper(*type))
      {
            case 'G':
                  for (i=0;i<256;i++)             /* default gray clut */
                  {
                        val = i + (i<<8);
                        clut[red][i] = val;
                        clut[green][i] = val;
                        clut[blue][i] = val;
                  }
                  break;
	    case 'D':				/* Disk File (IDX) clut */
		zvread( lutunit, idx_lut, NULL);
		for (j=0; j<3; j++)
			for (i=0;i<256;i++)
				 clut[j][i] = (int)257*idx_lut[i][j];
	
		zvclose( lutunit, NULL);
		  break;
	    default:
		  AbortMsg("Unknown lookup table type");
      }

}


void WriteTIFFClut(TIFF *tif)
{
      int i,j;
      enum {red,green,blue};
      uint16_t* clut[3];
      unsigned char idx_lut[256][4];
      int lutunit;
      uint16_t photo;

	TIFFGetField( tif, TIFFTAG_PHOTOMETRIC, &photo );
	if (photo==PHOTOMETRIC_PALETTE)
	{
		zvunit(&lutunit,"OUT",2, NULL);
		zvopen( lutunit, "op", "write", "u_nl", 1,
			 "u_ns", 1024, "u_format", "byte", NULL);
		
		TIFFGetField(tif, TIFFTAG_COLORMAP,
			 clut,clut+1,clut+2);
	
		/** create IDX format lut rgb0rgb0...**/
	
		memset( idx_lut, 0, 1024L );
		for (i=0; i<3; i++)
			for (j=0;j<256;j++)
				idx_lut[j][i] = clut[i][j]>>8;
		
		zvwrit( lutunit, idx_lut, NULL);
		zvclose( lutunit, NULL);
	}
	else zvmessage( "*** no lookup table in TIFF ***", " ");
}


char *tiff_time()
{
      static char time_string[20];
      time_t cur_time;
      struct tm timv;

      cur_time = time(0L);
      timv = *localtime(&cur_time);
      sprintf(time_string,"%4d:%02d:%02d %02d:%02d:%02d",
            1900+(timv.tm_year), 1+(timv.tm_mon), (timv.tm_mday),
                 (timv.tm_hour),   (timv.tm_min), (timv.tm_sec));
      
      return (time_string);
}



WriteTIFFData(unit, tif,nl,ns,nb)
int unit[];
TIFF *tif;
int nl,ns,nb;
{
      uint8_t *buf,*lbuf[3],*bptr[3],*ptr;
      uint32_t x,y,y1,y2,z;
      uint16_t planar;
      int nsamps;
      int tilebufsize;
      int tilepix;
      tiff_u_long tile_width,tile_height;
      int scanbytes;
      int band;
      int nrows;
      int chunky;
      int i;
      int plane_inc;
      int tile,tile1;
      long (*_tiff_write)();

      TIFFGetField( tif, TIFFTAG_PLANARCONFIG, &planar);
      chunky = (planar == PLANARCONFIG_CONTIG);

	
	if (TIFFIsTiled(tif))
	{
		TIFFWriteRawTile(tif, 0, (unsigned char *)&i, 0); /* forces setup */
		plane_inc = TIFFComputeTile(tif, 0,0,0,1)
				- TIFFComputeTile(tif, 0,0,0,0);
		TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_TILELENGTH, &tile_height);
		tilebufsize = TIFFTileSize(tif);
		_tiff_write = TIFFWriteEncodedTile;
	}
	else
	{
		TIFFWriteRawStrip(tif, 0, (unsigned char *)&i, 0); /* forces setup */
		plane_inc = TIFFComputeStrip(tif, 0, 1)
				- TIFFComputeStrip(tif, 0, 0);
		TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &tile_width);
		TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &tile_height);
		tilebufsize = TIFFStripSize( tif );
		_tiff_write = TIFFWriteEncodedStrip;
	}
	tilepix = tile_width*tile_height;
	buf = (uint8_t *)malloc(tilebufsize);
	if (chunky)
		lbuf[0] = (uint8_t *)malloc(tilebufsize);
	else
		lbuf[0] = (uint8_t *)malloc(tilebufsize*nb);
	for (i=1;i<nb;i++) 
		lbuf[i] = lbuf[0]+ i*tilepix;
	if (!buf || !lbuf[0]) 
		AbortMsg("failed to allocate buffer for tile/strip");
	
	tile=0;
	for (y = 0; y < nl; y+=tile_height)
	{
		nrows = MIN(tile_height, nl - y);
		for (x = 0; x < ns; x+=tile_width)
		{
			memset(buf,0,(size_t)tilebufsize);

			/* read in a single tile */
			nsamps = MIN(tile_width, ns - x);
			for (y1=0,y2=0; y1< nrows; y1++)
			{
				for (band=0;band<nb;band++)
				{
					zvread(unit[band],lbuf[band]+y2,
					"line",y+y1+1, "samp",x+1,"nsamps",
                                         nsamps, NULL);
				}
				y2+=tile_width;
			}

			if (chunky)
			{
				/* reshuffle pixels from chunky: rgbrgb...*/
				ptr=buf;
				for (i=0;i<nb;i++) bptr[i] = lbuf[i];
				for (z=0; z<tilepix; z++)
					for (band=0;band<nb;band++)
						*ptr++ = *bptr[band]++;

				if (_tiff_write(tif, tile++, buf, tilebufsize) < 0)
					goto bad;
			}
			else /* interleave by tile */
			{
				tile1=tile++;
				for (band=0; band<nb; band++)
				{
					if (_tiff_write(tif, tile1, lbuf[band], tilebufsize) < 0)
						goto bad;
					tile1 += plane_inc;
				}
			}

		}
	}
      
      free(buf);
      free(lbuf[0]);
      return (1);
bad:
      free(buf);
      free(lbuf[0]);
      return (0);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create optimal_color.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *    TABLE: -- Convert 24 bit RGB images into 8 bit pseudocolor
 *
 *         Description:
 *
 *            The TABLE module allows the creation of table structures
 *            which may be used to gather color histogram data for an
 *            RGB image, compute an optimal color table, and convert
 *            the RGB into an index-color table pair.
 *
 *         Color conversion Algorithm:
 *
 *            Currently the TABLE module converts RGB into a 12-bit
 *            packed integer, by taking the first 4 bits from each of
 *            the RGB byte values. As colors are added to the palette of
 *            the table, a histogram is kept of each color. When the
 *            process of adding colors is complete, the colors are sorted
 *            and the most common are picked for the color table. In addition,
 *            the color white and black are added to the table for completeness.
 * 
 *            A lookup table is used to map the 12-bit integers to the correct
 *            color table index value. The table is initialized by computing
 *            the index values for just the colors used to compute the histogram,
 *            and others are added as requests are made to compute indices for
 *            new RGB triplets.
 *
 *            Eventually, the optimization will be performed on a packed integer
 *            based on the YCbCr (luminance-chrominance) model, using seven bits
 *            for the Y component, and 4 each for the chrominance. This will
 *            be transparent to the client program, as the subroutine interface
 *            will be the same.
 *
 *         Compatibility:
 *  
 *            This code should work on either 2-byte or 4-byte int compilers
 *            and does not depend upon byte order, except that the temporary
 *            files derived from table_pack_rgb will have the 2-byte byte-order
 *            of the native machine, and so should not be moved to other hosts.
 *
 *         Revision History:
 *
 *            Original program by	Alan Mazer	fall 1988
 *	      Modularized               Niles Ritter	2 June 1993
 *	      Add new routines:		Niles Ritter	15 June 1993
 *		table_ncolors
 *		table_increment_color
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#ifndef NULL
#define NULL ((void*)0)
#endif

#define BITS_USED 4

/* useful macros */

#define RGB_INDEX( red,grn,blu )        \
       ( *(shift_left_red   + (red))  | \
         *(shift_left_green + (grn))  | \
         *(shift_left_blue  + (blu)) )
#define BYTE_COPY( src,dst,count) (memcpy((dst),(src),(count)))
#define BYTE_ZERO( src,count) (memset((src),0,(count)))

struct entry_def {
    unsigned char red,green,blue;    /* unpacked color: each ranges from 0 to 16 */
    unsigned int color;              /* packed color:  ranges from 0 to 4096    */
    long count;                      /* # times color found in sampling          */
};

struct table_def {
	struct entry_def *palette;   /* Palette of all colors found in image    */
	int *colormap;               /* Table mapping packed color to LUT index */
	int bits_used;               /* Number of bits used in packing (==4)    */
	int ncolors;                 /* Number of colors desired in LUT         */
	long length;                 /* Dimension of Palette (==4096)          */
};

/* public routines */
static public_routines(){}
int  table_new();
void table_free();
void table_add_colors();
long table_build();
void table_extract_lut();
void table_rgb_to_index();
void table_packed_to_index();
void table_pack_rgb();
void table_increment_color();
int  table_ncolors();



/* private routines */
static private_routines(){}
static void initialize_tables();
static long merge();
static long merge_new_index();
static int compare();
static int ShellSort();
void swap_entries();


/* private data */
static private_data(){}
static  int first_time=1;
static  long square[513];
static  unsigned int shift_left_red[256];
static  unsigned int shift_left_green[256];
static  unsigned int shift_left_blue[256];
static  unsigned int shift_left[256];



/**
 **  *****  PUBLIC IMPLEMENTATION ROUTINES  ******
 **/


/**
 **   table_new:
 **      creates new table structure.
 **      currently the table posesses a palette,
 **      used to gather the RGB color cube histogram,
 **      and a colormap array, which maps the packed
 **      RGB value to a sub-palette of <ncolors> colors.
 **
 **      Returns: 1 if successful, 0 if failure.
 **/



int table_new(  tab, ncolors )
int **tab;
int ncolors;
{
    int bits2;
    int i;
    int *colormap;
    long length;
    register int index;
    struct table_def *table=(struct table_def *)0;
    struct entry_def *palette;
    unsigned char pix_red,pix_green,pix_blue;

    if (first_time) initialize_tables();
    if (ncolors > 256 || ncolors < 0) goto failure;
    if (ncolors == 0) ncolors = 256;
   
             /* useful values */

    bits2 = 1<<BITS_USED;
    length = bits2 * bits2 * bits2;
 
	    /* allocate space for color table */

    table = (struct table_def *)calloc( 1L, sizeof( struct table_def) );
    if (!table) goto failure;
   
    table->length = length;
    table->bits_used = BITS_USED;  /* currently hardwired to 5 */
    table->ncolors = ncolors;
       

	/* initialize palette */

    table->palette = (struct entry_def *)calloc( 1L,
          sizeof(struct entry_def) * length);
    if (!table->palette) goto failure;
    palette = table->palette;

    for (pix_red=0;(int)pix_red < bits2; pix_red++)
      for (pix_green=0;(int)pix_green < bits2; pix_green++)
        for (pix_blue=0; (int)pix_blue < bits2; pix_blue++) {
            index = pix_red << 2*BITS_USED | pix_green << BITS_USED | pix_blue;
            (palette+index)->color = index;
            (palette+index)->red = pix_red;
            (palette+index)->green = pix_green;
            (palette+index)->blue = pix_blue;
        }

           /* init colormap to -1: flags that color is not mapped yet */
           
    colormap = (int *)malloc( sizeof(int) * length);
    table->colormap = colormap;
    if (!table->colormap) goto failure;
    for (i=0;i<length;i++)
       *colormap++ = -1;


           /* set pointer and return success flag */
    
    *tab = (int *)table;    
    return (1);
    
failure:
    table_free( table );
    return (0);
}



/**
 **   table_free: destruction method for table structure.
 **/


void table_free( table )
struct table_def *table;
{
    if (table)
    {
    	if (table->palette) free(table->palette);
    	if (table->colormap) free(table->colormap);
    	free (table);
    }
}


/**
 ** table_add_colors - adds the specified arrays of RGB
 **  pixels to the palette, for eventual sorting and
 **  optimization.
 **/

void table_add_colors(table, red, green, blue, npix)
struct table_def *table;
unsigned char *red,*green,*blue;
long npix;
{
    struct entry_def *palette = table->palette;
    register long pixel;
    register unsigned int color;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
 
	/*
         *   augment the color histogram with new colors
	 */

    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {
	color = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );
	(palette+color)->count++;
    }

}



/*
 * table_increment_color:
 *    increment the histogram count of single color specified.
 */

void table_increment_color(table, red, green, blue, number)
    struct table_def *table;
    unsigned char red;
    unsigned char green;
    unsigned char blue;
    long number;
{
    unsigned int color;
    struct entry_def *palette = table->palette;

        color = RGB_INDEX(red, green, blue);
        (palette+color)->count += number;
}



/*
 *  table_ncolors:
 *    return the number of lut colors set in table_new
 */


int table_ncolors( table )
struct table_def *table;
{
	return( table->ncolors );
}




/**
 **   table_build: gather color gamut information collected
 **     from the (red,grn,blue) arrays, and set up an
 **     optimal <ncolors>-color mapping for that set of colors.
 **     returns max error distance in colorspace.
 **/

long table_build( table )
struct table_def *table;
{
    int compare(),max_error;
    register long num_colors_found;
    struct entry_def *palette=table->palette,temp;
    long ncolors=table->ncolors;
    long length=table->length;
    register long source;

	/* compress palette by tossing out all colors with zero counts */

    source = 1;                  /* dont toss out black at 0 */
    palette[length-1].count = 1; /* dont toss out white, either */
    for (num_colors_found = 1;;num_colors_found++) {
	while ((palette+source)->count == 0 && source < length) source++;
	if (source == length) break;
	BYTE_COPY((char *)(palette+source),(char *)(palette+num_colors_found),
	    sizeof(*palette));
	source++;
    }
 
	    /*
	     *   Exclude BLACK and WHITE in the gamut from
	     *   the histogram sorting process. The palette will
	     *   eventually look like this:
	     *
	     *   WHITE, p[1], p[2], ... , BLACK, <everybody else>.
	     *
	     *    XXX-should also include points of extreme color.
	     */

	    /* sort the remainder to determine colors to use */

    ShellSort((char *)(palette+1),num_colors_found-2,
         (long)sizeof(*palette),compare);





	    /* 
	     * Force the Lightest color into the table 
	     * and swap BLACK and WHITE.
	     */


    /* Add WHITE to table */
    swap_entries(palette, ncolors-1, num_colors_found-1);
    
    /* swap WHITE <-> BLACK */
    swap_entries(palette, 0L, ncolors-1);
        
	    /* merge infrequent colors to nearest class */

    max_error = merge(table,num_colors_found);

    return (max_error);
}

/**
 ** table_extract_lut:
 **  return the lookup table used, in a public format
 **/
  
void table_extract_lut( table, red_lut, green_lut, blue_lut)
struct table_def *table;
unsigned char *red_lut,*green_lut,*blue_lut;
{
    int dn,bits_used=table->bits_used;
    register int ncolors = table->ncolors;
    register struct entry_def *palette = table->palette;  
	/* determine lookup table */

    for (dn=0;dn < ncolors;dn++) {
	red_lut[dn]   = shift_left[ (palette+dn)->red   ];
	green_lut[dn] = shift_left[ (palette+dn)->green ];
	blue_lut[dn]  = shift_left[ (palette+dn)->blue  ];
    }
}


/**
 **  table_rgb_to_index
 **   Compress RGB array to indexed value.
 **/


void table_rgb_to_index( table, red, green, blue, npix, index )
struct table_def *table;
unsigned char *red,*green,*blue;
long npix;
unsigned char *index;
{
    int ncolors = table->ncolors;
    unsigned char pix_red,pix_green,pix_blue;
    register long pixel;
    register long color;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
    register int *ColorMap = table->colormap;
    

	/* create new red, green, and blue planes */

    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {

        color = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );
	 
                /* If table hasn't seen this; add to ColorMap */
                
	if (*(ColorMap+color) < 0) merge_new_index( table, color );
	    
	*(index+pixel) = *(ColorMap+color);
    }
}


/**
 **  table_packed_to_index
 **   Compress 2-byte packed color to indexed value.
 **/


void table_packed_to_index( table, pack, npix, index )
struct table_def *table;
unsigned short *pack;
long npix;
unsigned char *index;
{
    int ncolors = table->ncolors;
    register long pixel;
    register long color;
    register unsigned short *color_ptr=pack;
    register int *ColorMap = table->colormap;
    

	/* create new red, green, and blue planes */

    for (pixel=0;pixel<npix;pixel++) {

        color = *color_ptr++;
	 
                /* If table hasn't seen this; add to ColorMap */
                
	if (*(ColorMap+color) < 0) merge_new_index( table, color );
	    
	*index++ = *(ColorMap+color);
    }
}


/**
 **  table_pack_rgb
 **   Compress RGB array to 2-byte packed representaton.
 **/


void table_pack_rgb( table, red, green, blue, npix, pack )
struct table_def *table;
unsigned char *red,*green,*blue;
long npix;
unsigned short *pack;
{
    register long pixel;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
    
    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {

        *pack++ = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );

    }
}


/**
 **  *****  PRIVATE IMPLEMENTATION ROUTINES  ******
 **/

/**
 **  swap_entries
 **    swaps  two color entries in a palette
 **/


void swap_entries(palette, index1, index2 )
struct entry_def *palette;
long index1;
long index2;
{
   struct entry_def temp;
  
   BYTE_COPY( palette+index1, &temp,          sizeof(temp));
   BYTE_COPY( palette+index2, palette+index1, sizeof(temp));
   BYTE_COPY( &temp,          palette+index2, sizeof(temp));
}



/**
 **  initialize_tables:
 **   sets up data arrays for module.
 **/

static void initialize_tables()
{
    register long int_temp;
 
    if (!first_time) return;
 
	/* make table for squares of differences */

    for (int_temp = -256;int_temp <= 256;int_temp++)
	square[int_temp+256] = int_temp*int_temp;

	/* make tables for right and left shifts */

    for (int_temp = 0;int_temp < 256;int_temp++) {
	shift_left_red[int_temp] =   (int_temp >> (8-BITS_USED)) << (2*BITS_USED);
	shift_left_green[int_temp] = (int_temp >> (8-BITS_USED)) << BITS_USED;
	shift_left_blue[int_temp] =  (int_temp >> (8-BITS_USED));
	
	shift_left[int_temp] =       (int_temp << (8-BITS_USED)) | int_temp;

    }
    
    first_time=0;
}



    /* Merge - takes color table generated by Build_Table and merges */
    /* infrequently used colors in with most popular.                */

static long merge(table,num_colors)
struct table_def *table;
long num_colors;
{
    int best_dist,best_index,dist;
    long unclassed_index;
    int j,max_error,ncolors=table->ncolors;
    register int *ColorMap = table->colormap;
    register struct entry_def *palette=table->palette;

	/* Initialize ColorMap output colors */

    for (j=0;j<ncolors;j++)
	*(ColorMap+ (palette[j].color) ) = j;

	/* do merge */

    max_error = 0;
    for (unclassed_index=ncolors;unclassed_index < num_colors;
	    unclassed_index++) {
	
	/*
	 * XXX for optimal performance, should #define a
	 * common loop for merge_new_index and this routine
	 * to save the stack-frame overhead.
	 */

	best_dist = merge_new_index(table, unclassed_index );
	if (best_dist > max_error) max_error = best_dist;

    }
    return(max_error);
}

/*
 * merge_new_index:
 *  merges a single color into the preset color table
 *  by finding the best matching color and then setting the
 *  new indexes colormap value to the best match.
 */

static long merge_new_index(table, unclassed_index )
struct table_def *table;
long unclassed_index;
{
    int best_index;
    long best_dist,dist;
    int j,ncolors=table->ncolors;
    register int *ColorMap = table->colormap;
    register long *square_off;
    register struct entry_def *palette=table->palette,*pal_ptr;
    register int red_diff,green_diff,blue_diff;
    register unsigned char pix_red,pix_green,pix_blue;

    square_off = &square[256];  /* squares of differences */

    /* do merge */
	
    pix_red = palette[unclassed_index].red;
    pix_green = palette[unclassed_index].green;
    pix_blue = palette[unclassed_index].blue;
    red_diff = pix_red - palette->red;
    green_diff = pix_green - palette->green;
    blue_diff = pix_blue - palette->blue;
    best_dist = *(square_off+red_diff) + *(square_off+green_diff) +
        *(square_off+blue_diff);
    best_index = 0;
    pal_ptr = palette+1;
    for (j=1;j < ncolors;j++) {
        red_diff = pix_red - pal_ptr->red;
        green_diff = pix_green - pal_ptr->green;
        blue_diff = pix_blue - pal_ptr->blue;
        dist = *(square_off+red_diff) + *(square_off+green_diff) +
           *(square_off+blue_diff);
        if (dist < best_dist) {
           best_dist = dist;
           best_index = j;
        }
        pal_ptr++;
    }
    
    *(ColorMap+(palette+unclassed_index)->color) = best_index;
    (palette+best_index)->count += (palette+unclassed_index)->count;
  
    return(best_dist);
}


/*
 * compare:
 *    ordering routine for sorting color entries by 
 * histogram count.
 */

static int compare(entry1,entry2)
struct entry_def *entry1,*entry2;
{
    int return_value;
    return_value = (entry1->count < entry2->count? 1:
	(entry1->count == entry2->count? 0:-1));
    return(return_value);
}

/*
 * ShellSort:
 *  A standard array-sorting algorithm.
 */

static int ShellSort(start, nelem, size, compar)
  char *start;		/* starting addr for sort	*/
  long	nelem;		/* number of elements to sort	*/
  long	size;		/* size of one element in bytes	*/
  int	(*compar)();	/* addr of compare routine	*/
{
  int	i,j,h;
  char  *v;

  v = malloc(size);
  if (v == 0) return 0;

  for (h = 1; h <= nelem; h = 3 * h + 1);
  while (h >= 3)
  {
    h /= 3;
    for (i = h; i < nelem; i++)
    {
      BYTE_COPY(start + size * i, v, size);
      j = i;
      while ((*compar)(start + size * (j - h), v) > 0)
      {
	BYTE_COPY(start + size * (j - h), start + size * j, size);
	j -= h;
	if (j < h) break;
      }
      BYTE_COPY(v, start + size * j, size);
    }
  }
  
  free (v);
  return 1;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create vtiff.pdf
process help=*
SUBCMD TOVIC	! convert from TIFF to vicar
    PARM INP	TYPE=STRING COUNT=1
    PARM OUT	TYPE=STRING COUNT=1:3
    PARM AUXIL  TYPE=STRING COUNT=0:2 DEFAULT=--
    PARM INFO   TYPE=KEYWORD VALID=(DUMP,NODUMP) DEFAULT=NODUMP
!	PARM SIZE	TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
!	PARM SL		TYPE=INTEGER COUNT=1 DEFAULT=1
!	PARM SS		TYPE=INTEGER COUNT=1 DEFAULT=1
!	PARM NL		TYPE=INTEGER COUNT=1 DEFAULT=0
!	PARM NS		TYPE=INTEGER COUNT=1 DEFAULT=0
END-SUBCMD

SUBCMD-DEFAULT FROMVIC	! convert from vicar to TIFF
    PARM INP 		TYPE=STRING COUNT=1:3
    PARM OUT 		TYPE=STRING COUNT=1
    PARM AUXIL 		TYPE=STRING COUNT=0:2 DEFAULT=--
    PARM TLINES		TYPE=INTEGER DEFAULT=128
    PARM TSAMPS		TYPE=INTEGER DEFAULT=128
    PARM COMPRESS	TYPE=KEYWORD VALID=(LZW,PACKBITS,NOCOMP) DEFAULT=NOCOMP
    PARM COLORBITS  TYPE=KEYWORD VALID=(BIT8,BIT24) DEFAULT=BIT24
    PARM TIFFMODE	TYPE=KEYWORD VALID=(TILED,STRIPS) DEFAULT=STRIPS
    PARM INTRLEAV	TYPE=KEYWORD VALID=(CHUNKY,PLANAR) DEFAULT=CHUNKY
    PARM LAT		TYPE=REAL COUNT=0:3 DEFAULT=--
    PARM HEMILAT	TYPE=KEYW VALID=(NORTH,SOUTH) DEF=NORTH
    PARM LONG		TYPE=REAL COUNT=0:3 DEFAULT=--
    PARM HEMILONG	TYPE=KEYW VALID=(EAST,WEST) DEF=WEST
    PARM HDEGPIX	TYPE=REAL COUNT=0:3 DEFAULT=(0,0,1)
    PARM VDEGPIX	TYPE=REAL COUNT=0:3 DEFAULT=(0,0,1)
    PARM XPIXPOS	TYPE=INTEGER COUNT=1 DEFAULT=0
    PARM YPIXPOS	TYPE=INTEGER COUNT=1 DEFAULT=0
    PARM RESUNIT	TYPE=KEYWORD VALID=(INCH,CENTIMETER) DEF=INCH
    PARM XRES      	TYPE=REAL COUNT=1 DEF=72.0
    PARM YRES		TYPE=REAL COUNT=1 DEF=72.0
    
END-SUBCMD

!# annot function="Importing and Exporting Data"
!# annot keywords=(convert,TIFF,RGB,thumbnail)

end-proc
.TITLE
Converts images between VICAR and TIFF format.
.HELP
   VTIFF is a Vicar2 program, which can be used to convert
   between VICAR labeled images and TIFF format files, using
   either scanline (strip) organization, or the newer Revision
   6.0 TIFF tiled-image format. Currently grayscale, image-
   lookup table pairs and RGB triplets are supported. In addition,
   multiple auxiliary images may be placed in the same file,
   such as "thumbnail" preview images.

   "TIFF" is a trademark of Aldus Corporation, and is a public
   standard for platform-independent raster-image storage and
   transmission. It is an acronym for Tagged Image File Format.

.PAGE
CALLING SEQUENCE:

	vtiff-tovic   image.tiff  image 	  AUXIL=(files...)
	vtiff-tovic   image.tiff  (image, lookup) AUXIL=(files...)
	vtiff-tovic   image.tiff  (red, grn, blu) AUXIL=(files...)

	vtiff-fromvic  image  		image.tif  [options..]
	vtiff-fromvic  (image, lookup)  image.tif  [options..]
	vtiff-fromvic  (red, grn, blu)  image.tif  [options..]

   where image.tiff is TIFF format, and lookup is a
   color lookup table in IDX format (1024 sample, 1 line
   byte image, with values r1,g1,b1,0,r2,g2,b2,0,...).

   Unlike VMAC, (which this program is intended to partially replace
   as a Unix-portable TIFF converter), no support exists yet to convert
   an RGB triplet into an 8-bit image-lookup pair.

.PAGE
OPERATION

  In the "tovicar" mode, the program unpacks the image and
  lookup table, if desired. It will also unpack any additional
  files and save them as specified by the AUX parameter.

  In the "fromvicar" mode, you have the option of saving the
  files in strip-mode (horizontal scanlines contiguous), which
  makes them TIFF 5.0 compliant; or using the TIFF 6.0 tiling
  option, which breaks the image up into rectangular tiles, and
  saves those in a contiguous manner.

  You may save multiple additional images in the same file using
  the "AUX" parameter. If the file uses a lookup table, you may
  include this in the second INP parameter. It currently must be
  an IDX-format lookup table (1 line x 1024 sample byte image:
  r1,g1,b1,0,r2..). NOTE: Tiling is a new feature, which many
  TIFF programs cant yet handle. If so, use the default strip-mode. 
.PAGE
OPERATION

  You may also chose a TIFF data compression option. The
  currently supported keywords in VTIFF are 'LZW = lempel-ziv-welch
  compression, 'PACKBITS = Macintosh-style packbits (run-length
  encoding) compression, or 'NOCOMP = no compression. NOTE: The
  TIFF 6.0 standard now discourages the use of LZW, as Unisys Corp.
  claims to have a patent on the algorithm, and so may not
  support LZW in the future.

  For RGB triplets, whether tiled or in strips, you may choose
  to organize the data using 'CHUNKY or 'PLANAR interleaving.
  The 'CHUNKY format is analogous to the VICAR BIP (Band-interleaved
  by pixel) organization, and is the default. The 'PLANAR is
  similar to BSQ (Band-sequential) format, and while this permits
  faster conversion and extraction, it is an extension to TIFF
  that is not supported by as many TIFF-compatible programs.
.PAGE
OPERATION

  The latitude-longitude parmeters are extensions to TIFF to
  permit specification of scanned map georeference data. These
  extensions are TIFF-compliant, and will not interfere with
  any standard TIFF-reading program, which will ignore the 
  extended data fields. For more information on the cartographic
  TIFF extensions, contact the Cartographic Applications Group
  at JPL. Do not use these parameters if you only want standard TIFF.

  In general, for the most easily exportable TIFF file, use as many
  of the default values as possible. The parameters are set up so
  that the parameters further down the list will cause the file to
  be less exportable if their default values are overridden.

.PAGE

REVISION HISTORY


   Written by:            N. D. Ritter  September 1991
   Cognizant Programmer:  N. D. Ritter  1991-1992
   Latest revision:       B             March 1993.

REFERENCES

   "TIFF" Revision 6.0, Final - Jun 3, 1992,
      Aldus Developers Desk, available via anonymous ftp
      through sgi.com.
.PAGE
AKNOWLEDGMENT

   This program is a VICAR front-end to a public-domain
   subroutine library of TIFF file format routines, written
   by Sam Leffler, and extended for JPL use by Niles Ritter.
   The source code carries the following copyright notice:

   <quote>
   Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
   Copyright (c) 1991, 1992 Silicon Graphics, Inc.
 
   Permission to use, copy, modify, distribute, and sell this software and 
   its documentation for any purpose is hereby granted without fee, provided
   that (i) the above copyright notices and this permission notice appear in
   all copies of the software and related documentation, and (ii) the names of
   Sam Leffler and Silicon Graphics may not be used in any advertising or
   publicity relating to the software without the specific, prior written
   permission of Sam Leffler and Silicon Graphics.
.PAGE
AKNOWLEDGMENT
   
   THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
   EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
   WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
   
   IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
   ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
   WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
   LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
   OF THIS SOFTWARE.

   <unquote>.

.LEVEL1


.SUBCMD TOVIC
Convert TIFF format file
to VICAR.
.VAR INP -TOVIC
Input TIFF file
.VAR OUT -TOVIC
Output VICAR file.
.VAR AUXIL -TOVIC
Auxiliary files to pull
out of TIFF file.
.VAR INFO -TOVIC
Print the TIFF Label?
.VAR SIZE -TOVIC
.VAR SL -TOVIC
.VAR SS -TOVIC
.VAR NL -TOVIC
.VAR NS -TOVIC

.SUBCMD FROMVIC
Covert from VICAR image
to TIFF format file.
.VAR INP -FROMVIC
Input VICAR file
.VAR OUT -FROMVIC
Output TIFF file
.VAR AUXIL -FROMVIC
Auxiliary files to stuff
into output TIFF file.
.VAR TLINES -FROMVIC
Number of lines in tile
.VAR TSAMPS -FROMVIC
Number of samps in tile
.VAR COMPRESS -FROMVIC
Compression type
.VAR COLORBITS  -FROMVIC 
Convert RGB to 8-bit?
.VAR TIFFMODE -FROMVIC
Use Strips or Tiles ?
.VAR INTRLEAV -FROMVIC
Chunky Interleave:rgbrgb...?
.VAR LUT -FROMVIC
Type of Color Lookup 
Table to use.
.VAR LAT -FROMVIC
Ref. Latitude in (D,Min,Sec).
.VAR HEMILAT -FROMVIC
North or South Latitude ?
.VAR LONG -FROMVIC
Ref. Longitude in (D,Min,Sec).
.VAR HEMILONG -FROMVIC
East or West Longitude ?
.VAR HDEGPIX -FROMVIC
Horiz. (D,Min,Sec) per pixel.
.VAR VDEGPIX -FROMVIC
Vert. (D,Min,Sec) per pixel.
.VAR XPIXPOS -FROMVIC
X-Position of Coord. axis.
.VAR YPIXPOS -FROMVIC
Y-Position of Coord. axis.
.VAR RESUNIT -FROMVIC
Units used in X/YRES
.VAR XRES -FROMVIC
#pixels per (RESUNIT) horiz.
.VAR YRES -FROMVIC
#pixels per (RESUNIT) vert.

.level2


.SUBCMD TOVIC
Convert TIFF format file
to VICAR.
.VAR INP -TOVIC
Input TIFF file
.VAR OUT -TOVIC
Output VICAR file.
.VAR AUXIL -TOVIC
Auxilary files to pull
out of TIFF file.

.SUBCMD FROMVIC
Covert from VICAR image
to TIFF format file.
.VAR INP -FROMVIC
Input VICAR file
.VAR OUT -FROMVIC
Output TIFF file
.VAR AUXIL -FROMVIC
Auxilary files to stuff
into output TIFF file.
.VAR TLINES -FROMVIC
Number of lines in tile.
.VAR TSAMPS -FROMVIC
Number of samps in tile.
.VAR COMPRESS -FROMVIC
Use Compression ?
.VAR COLORBITS  -FROMVIC 
When an (r,g,b) triplet is input, tells VTIFF whether the
output TIFF file should be 8-bit ('BIT8) or 24-bit (BIT24) color .
.VAR LUT -FROMVIC
Type of Color Lookup 
Table to use.
.VAR LAT -FROMVIC
Ref. Latitude in DMS.
.VAR LONG -FROMVIC
Ref. Longitude in DMS.
.VAR HDEGPIX -FROMVIC
Horiz. (D,Min,Sec) per pixel.
.VAR VDEGPIX -FROMVIC
Vert. (D,Min,Sec) per pixel.
.VAR XPIXPOS -FROMVIC
X-Position of Coord. axis.
.VAR YPIXPOS -FROMVIC
Y-Position of Coord. axis.
.VAR RESUNIT -FROMVIC
Units used in X/YRES to expression resolution
of displayed map/image.
.VAR XRES -FROMVIC
#pixels per (RESUNIT) horizontal.
.VAR YRES -FROMVIC
#pixels per (RESUNIT) vertical.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstvtiff.pdf
procedure
refgbl $autousage
body

let $autousage="none"

write "each of the difpic calls should yield zero differences"

gen r 50 50
vtiff r r.tif
vtiff-tovic r.tif r1
difpic (r,r1)

vtiff r r.tif 'packbits
vtiff-tovic r.tif r1
difpic (r,r1)

vtiff r r.tif 'lzw
vtiff-tovic r.tif r1
difpic (r,r1)

vtiff r r.tif 'tiled
vtiff-tovic r.tif r1
difpic (r,r1)

vtiff r r.tif 'tiled 'lzw
vtiff-tovic r.tif r1
difpic (r,r1)

f2 out=lut fun="(samp %256)*(samp%4)" nl=1 ns=1024
vtiff (r,lut) lut.tif
vtiff-tovic lut.tif (r1,lut1)
difpic (r,r1)
difpic (lut,lut1)

gen g 50 50 linc=0
gen b 50 50 sinc=0

vtiff (r,g,b) color.tif
vtiff-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff (r,g,b) color.tif 'planar
vtiff-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff (r,g,b) color.tif 'tiled
vtiff-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff (r,g,b) color.tif 'planar 'tiled
vtiff-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff (r,g,b) color.tif 'planar 'tiled 'lzw
vtiff-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

gen a nl=500 ns=500
vtiff a a.tif 'tile tline=128 tsamp=128 'packbits
vtiff-tovic a.tif a1
difpic (a,a1)

!test dump facility:
vtiff-tovic a.tif a1 'dump

end-proc
$ Return
$!#############################################################################
$Imake_File:
$ create vtiff.imake

#define PROGRAM vtiff

#define MAIN_LANG_C
#define USES_C
#define R2LIB

#define LIB_RTL
#define LIB_TAE

#define INCLUDE_LIST tiff.h tiffio.h prototypes.h tiffcompat.h tiffiop.h
#define MODULE_LIST \
vtiff.c \
optimal_color.c \
t_aux.c \
t_cmpat.c \
t_cmpr.c \
t_dirr.c \
t_dirw.c \
t_diri.c \
t_dump.c \
t_err.c

#define MODULE_LIST2 \
t_file.c \
t_lzw.c \
t_mach.c \
t_pack.c \
t_prnt.c \
t_strp.c \
t_swab.c \
t_tile.c \
t_vers.c

#define DEBUG	/* comment out on delivery */
$ Return
$!#############################################################################
