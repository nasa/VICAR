$!****************************************************************************
$!
$! Build proc for MIPL module bigmalloc
$! VPACK Version 1.9, Monday, December 07, 2009, 16:07:53
$!
$! Execute by entering:		$ @bigmalloc
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module bigmalloc ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to bigmalloc.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("bigmalloc.imake") .nes. ""
$   then
$      vimake bigmalloc
$      purge bigmalloc.bld
$   else
$      if F$SEARCH("bigmalloc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bigmalloc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bigmalloc.bld "STD"
$   else
$      @bigmalloc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bigmalloc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bigmalloc.com -mixed -
	-s bigmalloc_vms.c bigmalloc_unix.c -
	-i bigmalloc.imake -
	-t tbigmalloc.c tbigmalloc.imake tbigmalloc.pdf tstbigmalloc.pdf -
	-o bigmalloc.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bigmalloc_vms.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <rms.h>
#include <ssdef.h>
#include <secdef.h>

#ifndef NULL
#define NULL ((void*)0)
#endif

static struct bigmem_ctrl {
  struct bigmem_ctrl *next;
  unsigned char *start;
  unsigned char *end;
  int channel;
} *bigmem_head = NULL;

unsigned char *malloc();

/************************************************************************/
/* BigMalloc() allocates a large section of memory by setting up an alternate
 * paging file.This is needed to get around the PAGFIL quota limit in VMS.
 * It is called exactly like malloc().   This alternate page file is put
 * on the disk that is pointed at by the logical name "v2$scratch".  Note
 * that v2$scratch can point to a directory or even a file; only the disk
 * part of the name is used.  If there is not enough room on the v2$scratch
 * disk, then normal malloc is tried.  If this fails, NULL is returned.
 * BigMallocForce() will always attempt to set up the paging file.  BigMalloc()
 * will only do so if the requested size is over one megabyte, otherwise
 * it will use normal malloc().  Most applications should call BigMalloc()
 * instead of BigMallocForce().  BigFree() is used to free the memory obtained
 * by these routines.
 *
 * The return value for these routines is a pointer to the requested area,
 * or NULL if the allocation failed.
 *
 * The file created on the v2$scratch disk is a temporary file with no
 * directory entry.  It will never show up in a directory, and is automatically
 * deleted when the file is closed.
 */

unsigned char *BigMalloc(size)
  int size;			/* in: # of bytes to allocate		*/
{
  unsigned char *BigMallocForce();
  unsigned char *addr;

  if (size <= 1024*1024) {	/* smaller than a megabyte, so use malloc() */
    addr = malloc(size);
    if (addr)
      return addr;
  }			/* if small malloc() failed, try BigMalloc anyway */
  return BigMallocForce(size);
}


unsigned char *BigMallocForce(size)
  int size;			/* in: # of bytes to allocate		*/
{
  int status;
  int blocks;
  unsigned char *s[2], *r[2];
  int flags;
  struct bigmem_ctrl *mem;
  struct FAB fab;

  mem = malloc(sizeof(struct bigmem_ctrl));
  if (mem == NULL)
    return malloc(size);		/* not likely */

  blocks = (size+511) / 512;

  fab = cc$rms_fab;
  fab.fab$l_fna = "v2$scratch:vidstemp.pgfl";
  fab.fab$b_fns = strlen(fab.fab$l_fna);
  fab.fab$w_mrs = 512;
  fab.fab$b_rfm = FAB$C_FIX;
  fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
  fab.fab$l_fop = FAB$M_UFO | FAB$M_MXV | FAB$M_TMD;	/* user-file-open, */
			/* maximize version #,temporary file delete-on-close */
  fab.fab$l_alq = blocks;

  status = sys$create(&fab);

  if (status != RMS$_NORMAL && status != RMS$_CREATED) {
    free(mem);
    return malloc(size);		/* try a normal alloc */
  }

  mem->channel = fab.fab$l_stv;		/* save channel */

  r[0] = r[1] = 10;	/* Having these addresses the same causes the	*/
			/* program region to be expanded (10 is just an	*/
			/* arbitrary number < p1 region			*/

  flags = SEC$M_EXPREG | SEC$M_WRT;   /* expand program region, allow writes */

  status = sys$crmpsc(r,s,0,flags,0,0,0,mem->channel,blocks,0,0,0); /* map it */

  mem->start = s[0];
  mem->end = s[1];

  if ((status != SS$_NORMAL) && (status != SS$_CREATED)) {
    status = sys$dassgn(mem->channel);
    free(mem);
    return malloc(size);		/* try a normal malloc */
  }

  mem->next = bigmem_head;
  bigmem_head = mem;

  return mem->start;

}

/************************************************************************/
/* BigFree() frees a section of memory allocated by BigMalloc() or
 * BigMallocForce().  It is called exactly like free().  There is no
 * return value or error indication.
 */

void BigFree(addr)
  unsigned char *addr;		/* in: address to free */
{
  int status;
  unsigned char *r[2];
  struct bigmem_ctrl *mem, *oldmem;

  oldmem = NULL;
  for (mem = bigmem_head; mem != NULL; mem = mem->next) {
    if (mem->start == addr)
      break;
    oldmem = mem;
  }

  if (mem == NULL) {		/* whoops, address not allocated */
    free(addr);			/* maybe it was allocated normally */
    return;			
  }

  if (oldmem == NULL)		/* remove this struct from linked list */
    bigmem_head = mem->next;
  else
    oldmem->next = mem->next;

  r[0] = mem->start;
  r[1] = mem->end;

  status = sys$deltva(r, 0, 0);		/* delete the mapped section */

  status = sys$dassgn(mem->channel);	/* close the file (which deletes it) */

  free(mem);

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create bigmalloc_unix.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdlib.h>

/************************************************************************/
/* BigMalloc() and BigMallocForce() are currently stubs under Unix which
 * merely call malloc().  The routines are mainly needed for VMS.
 * A Unix implementation should be done in the future.
 */

unsigned char *BigMalloc(size)
  int size;			/* in: # of bytes to allocate		*/
{

  return malloc(size);
}


unsigned char *BigMallocForce(size)
  int size;			/* in: # of bytes to allocate		*/
{

  return malloc(size);
}


/************************************************************************/
/* BigFree() frees a section of memory allocated by BigMalloc() or
 * BigMallocForce().  It is called exactly like free().  There is no
 * return value or error indication.
 */

void BigFree(addr)
  unsigned char *addr;		/* in: address to free */
{

  free(addr);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bigmalloc.imake
#define SUBROUTINE bigmalloc

#if VMS_OS
#define MODULE_LIST bigmalloc_vms.c
#define CLEAN_OTHER_LIST bigmalloc_unix.c
#else
#define MODULE_LIST bigmalloc_unix.c
#define CLEAN_OTHER_LIST bigmalloc_vms.c
#endif

#define USES_C

#define P2_SUBLIB

$ Return
$!#############################################################################
$Test_File:
$ create tbigmalloc.c
#include "vicmain_c"

unsigned char *BigMalloc(), *BigMallocForce();
void BigFree();

main44()
{
   unsigned char *ptr;
   int i;
   void try();

   try(1024, 0);

   try(1024 * 1024, 0);

   try(1024 * 4096, 0);

   try(1024, 1);
}

void try(size, flag)
int size, flag;
{
   unsigned char *ptr;
   unsigned int i;
   unsigned char tmp;
   char msg[256];

   if (flag)
      ptr = BigMallocForce(size);
   else
      ptr = BigMalloc(size);

   if (ptr == NULL) {
      sprintf(msg,
	   "BigMalloc failed for size %d.  This may or may not indicate test",
	   size);
      zvmessage(msg, "");
      zvmessage(
	   "failure.  If on VMS, check for space available on v2$scratch and try again","");
      return;
   }


   for (i=0; i<size; i++)
      *(ptr+i) = i % 256;
   for (i=0; i<size; i++) {
      tmp = i%256;
      if (*(ptr+i) != tmp) {
         sprintf(msg, "Compare error in size %d, location %d, value=%d, should be %d",
		size, i, *(ptr+i), tmp);
         zvmessage(msg, "");
         zvmessage("Test failed", "");
         return;
      }
   }

   for (i=0; i<size; i++)
      *(ptr+i) = ~ (i % 256);
   for (i=0; i<size; i++) {
      tmp = ~ (i % 256);
      if (*(ptr+i) != tmp) {
         sprintf(msg, "Compare inv error in size %d, location %d, value=%d, should be %d",
		size, i, *(ptr+i), tmp);
         zvmessage(msg, "");
         zvmessage("Test failed", "");
         return;
      }
   }

   BigFree(ptr);

   sprintf(msg, "Test of size %d succeeded", size);
   zvmessage(msg, "");

}

$!-----------------------------------------------------------------------------
$ create tbigmalloc.imake
#define PROGRAM tbigmalloc

#define MODULE_LIST tbigmalloc.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

$!-----------------------------------------------------------------------------
$ create tbigmalloc.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstbigmalloc.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
tbigmalloc
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create bigmalloc.hlp
1 BIGMALLOC

  unsigned char *BigMalloc(int size);
  unsigned char *BigMallocForce(int size);
  void BigFree(unsigned char *addr);

  These functions allocate and free large chunks of memory.  They are
  used just like the C library malloc() and free() routines.

  BigMalloc() checks the requested size.  If it is greater than a threshhold
  size (currently one megabyte), then BigMallocForce is called to allocate
  the memory.  If it is less than or equal to the threshhold, then the
  standard malloc() routine is called instead.

  BigMallocForce() allocates the area as described below regardless of the
  threshhold value.  Since there is some overhead involved, it is best not
  to use this routine directly, but instead to let BigMalloc() decide whether
  it is worth using the BigMalloc scheme or not.

  Both BigMalloc() and BigMallocForce() return a pointer to the requested
  memory area.  If the BigMalloc scheme fails, then the standard malloc()
  routine will be tried.  If that fails also, then NULL is returned.

  BigFree() frees the memory allocated by BigMalloc() or BigMallocForce().
  Only BigFree should be use to free this memory; do not call free() directly.

  Note:  The function return types should be declared in prototypes so the
  C compiler will treat them correctly.  The address returned may be cast
  into any type of pointer.

  There is no Fortran bridge to these routines since Fortran does not
  understand pointers.

  VMS implementation
  ------------------

  The reason these functions exists stems from VMS.  Programs are limited
  in the amount of the system paging file they can use by PGFLQUO (the
  account's page file quota).  However, the available virtual address space
  is typically much larger than the page file quota.  BigMalloc gets around
  the page file quota limitation by creating a temporary file for use as
  a paging file for the requested memory.  Since the memory does not use
  the system paging file, the page file quota is not charged.

  The limitation of this scheme is, of course, that there must be disk space
  available to create the temporary paging file.  The temporary file is
  created on the disk that is pointed at by the logical name "v2$scratch".
  The directory and/or file portion of the logical name are not used; only
  the disk.  The file is a temporary file with no directory entry, so it is
  invisible to any other program.  The file is automatically deleted when
  it is closed.  If there is not enough space on the requested disk, then
  BigMalloc will try to call malloc() instead.

  If the BigMalloc fails, there are three basic reasons (in order of
  probability):  1) The disk pointed at by v2$scratch does not have enough
  space,  2) v2$scratch is not defined or is defined to a non-existent disk,
  or 3) the virtual address space for the process is full.

  Unix implementation
  -------------------

  Currently, these routines simply call malloc() and free() under Unix,
  as no mechanism similar to VMS has yet been identified.  However,
  application programs should still use BigMalloc to allocate large chunks
  of memory anyway, because the routines may be implemented in the future
  under Unix, and they could be necessary if the program is run under VMS.

2 History

  Original Programmer:  Bob Deen 08-89
  Current Cognizant Programmer:  Bob Deen  04-93
  Source Language:  C
$ Return
$!#############################################################################
