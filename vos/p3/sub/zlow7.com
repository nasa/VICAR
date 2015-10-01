$!****************************************************************************
$!
$! Build proc for MIPL module zlow7
$! VPACK Version 1.5, Friday, March 19, 1993, 20:17:05
$!
$! Execute by entering:		$ @zlow7
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
$ write sys$output "*** module zlow7 ***"
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
$   if F$SEARCH("zlow7.imake") .nes. ""
$   then
$      vimake zlow7
$      purge zlow7.bld
$   else
$      if F$SEARCH("zlow7.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zlow7
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zlow7.bld "STD"
$   else
$      @zlow7.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zlow7.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zlow7.com -
	-s zlow7.c -
	-i zlow7.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zlow7.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ftnname.h"
#if 0
#include "low7.h"
#endif

int low7();

void zlow7(c0, c1, c2, c2a, c2b, c2c, c2d, c3, c3a, c3b, c4)
   struct card0   *c0;
   struct card1   *c1;
   struct card2   *c2;
   struct card2a  *c2a;
   struct card2b  *c2b;
   struct card2c  *c2c;
   struct card2d  *c2d;
   struct card3   *c3;
   struct card3a  *c3a;
   struct card3b  *c3b;
   struct card4   *c4;
{

/* NOTE:  This does not work.  The low7.h include file is missing, as */
/* is low7() itself.  Therefore I am simply commenting out the entire */
/* routine.  If it is ever resurrected, this can be restored. rgd 3/2010 */

#if 0
   FTN_NAME(low7)(
      c0->outfile, c0->tabfile,&c0->h2ofac,&c0->co2fac,&c0->o3fac,&c0->o2fac,
      &c0->ch4fac,&c0->so2fac, c0->rbuf, c0->tbuf, c0->sbuf,
      &c1->model,&c1->itype,&c1->iemsct,&c1->imult,&c1->m1,&c1->m2,&c1->m3,
      &c1->m4,&c1->m5,&c1->m6,&c1->mdef,&c1->im,&c1->noprt,&c1->tbound,
      c1->specalb, 
      &c2->ihaze,&c2->iseasn,&c2->ivulcn,&c2->icstl,&c2->icld,&c2->ivsa,
      &c2->vis,&c2->wss,&c2->whh,&c2->rainrt,&c2->gndalt, 
      &c3->h1, &c3->h2, &c3->angle, &c3->range, &c3->beta, &c3->ro, &c3->len, 
      &c4->v1, &c4->v2, &c4->dv, 
      &c2a->cthik, &c2a->calt, &c2a->cext, &c2a->iseed, 
      &c2b->zcvsa, &c2b->ztvsa, &c2b->zinvsa,
      &c2c->ml,&c2c->ird1,&c2c->ird2, c2c->title, c2c->zmdl, c2c->p, c2c->tx,
      c2c->wmol, c2c->jchar, c2c->ahaze, c2c->eqlwcz, c2c->rratz, c2c->iha1,
      c2c->icld1, c2c->ivul1, c2c->isea1, c2c->ichr1,
      c2d->ireg,c2d->awccon,c2d->title,c2d->vx,c2d->extc,c2d->absc, c2d->asym,
      &c3a->iparm,&c3a->iph,&c3a->iday,&c3a->isourc,&c3a->parm1,&c3a->parm2,
      &c3a->parm3,&c3a->parm4,&c3a->time,&c3a->psipo,&c3a->anglem,&c3a->g,
      &c3b->nangls, c3b->angf, c3b->f);
#endif

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create zlow7.imake

#define SUBROUTINE zlow7

#define MODULE_LIST zlow7.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
