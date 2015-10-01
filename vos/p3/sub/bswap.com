$!****************************************************************************
$!
$! Build proc for MIPL module bswap
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:43
$!
$! Execute by entering:		$ @bswap
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
$ write sys$output "*** module bswap ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("bswap.imake") .nes. ""
$   then
$      vimake bswap
$      purge bswap.bld
$   else
$      if F$SEARCH("bswap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bswap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bswap.bld "STD"
$   else
$      @bswap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bswap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bswap.com -
	-s bswap.c -
	-i bswap.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bswap.c
$ DECK/DOLLARS="$ VOKAGLEVE"
static char sccsBswap[] = "@(#)  bswap.c 2.2 9/9/88 ";
/* bswap(in,n)    swap byte pairs

     in = buffer holding the byte pairs
     n  = number of byte pairs to swap

  ORIGINAL CODE: 

  .TITLE	BSWAP
  .PSECT	BSWAP
  .ENTRY	BSWAP,^M<R2,R3>
  ;	IN	R2
  ;	N	R3
	MOVL     4(AP),R2
        MOVL    @8(AP),R3
        MOVL	#2,R1
  ;
  LOOP:	MOVB	1(R2),R0
	MOVB	(R2),1(R2)
	MOVB	R0,(R2)
	ADDL2   R1,R2
	SOBGTR	R3,LOOP
	RET
  .END

  NEW CODE: 
*/

bswap_(in,n)
     char in[];
     int *n;
{
  bswap(in,*n);
}

bswap(in,n)
     char in[];
     int  n;
{
  int register i, indx;
  char register temp;

  for (i=0;i<n;indx=i*2,i++) {
    temp = in[indx];
    in[indx] = in[indx+1];
    in[indx+1] = temp;
  }
}
    
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bswap.imake

#define SUBROUTINE bswap

#define MODULE_LIST bswap.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
