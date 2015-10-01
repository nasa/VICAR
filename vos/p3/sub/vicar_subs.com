$!****************************************************************************
$!
$! Build proc for MIPL module vicar_subs
$! VPACK Version 1.9, Thursday, January 25, 2007, 09:35:32
$!
$! Execute by entering:		$ @vicar_subs
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
$ write sys$output "*** module vicar_subs ***"
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
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vicar_subs.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
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
$   if F$SEARCH("vicar_subs.imake") .nes. ""
$   then
$      vimake vicar_subs
$      purge vicar_subs.bld
$   else
$      if F$SEARCH("vicar_subs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vicar_subs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vicar_subs.bld "STD"
$   else
$      @vicar_subs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vicar_subs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vicar_subs.com -mixed -
	-s vicar_subs.c -
	-i vicar_subs.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vicar_subs.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "parm.h"
#include "const.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void l_message();

int l_get_value (par)
   struct parm *par;
{
   char msg[80];
   int status, i, defaulted, length, count;

   /*---- see how many of the parameter have been specified -----------------*/
   status = zvpcnt (par->name, &par->count);

   /*---- maxcnt == 0 means there is no limit on count ----------------------*/
   if (par->maxcnt && par->count > par->maxcnt) {
      sprintf (msg, " too many of par %s  specified", par->name);
      l_message (msg);
      exit(0);
   }

   /*---- length tells zvparm how long the strings being passed in are ------*/
   if (EQUAL(par->type,"STRING"))
      length = MAX_PAR_STRING;
   else
      length = 0;

   /*---- grab space for parameter values depending on type and count -------*/
   if (EQUAL(par->type,"STRING")) {
      par->val.c = (char *)get_space(par->count*length*sizeof(char));
      status = zvparm (par->name,par->val.c,&par->count,
                                         &defaulted,par->maxcnt,length);
      count = par->count;
      for (i=0; i<count; i++) {
         if (strlen(par->val.c + i*MAX_PAR_STRING) == 0)
             par->count -= 1;
      }
   }
   else if (EQUAL(par->type,"INT")) {
      par->val.i = (int *)get_space(par->count * sizeof(int));
      status = zvparm (par->name,par->val.i,&par->count,
                               &defaulted,par->maxcnt,length);
   }
   else if (EQUAL(par->type,"REAL")) {
      par->val.f = (float *)get_space(par->count * sizeof(float));
      status = zvparm (par->name,par->val.f,&par->count,
                                   &defaulted,par->maxcnt,length);
   }
   else if (EQUAL(par->type,"DOUB")) {
      par->val.d = (double *)get_space(par->count * sizeof(double));
      status = zvparmd (par->name,par->val.d,&par->count,
                                     &defaulted,par->maxcnt,length);
   }
}

int l_list_value (par)
   struct parm *par;
{
   char msg[256], value[200];
   int i;

   memset (msg,'\0',sizeof(msg));
   sprintf (msg," %10s = ",par->name);
   for (i=0; i<par->count; i++) {
      memset (value,'\0',sizeof(value));

      if (EQUAL(par->type,"STRING"))
         sprintf (value, "%s", (par->val.c + i*MAX_PAR_STRING));
      else if (EQUAL(par->type,"INT"))
         sprintf (value, "%d", par->val.i[i]);
      else if (EQUAL(par->type,"REAL"))
         sprintf (value, "%g", par->val.f[i]);
      else if (EQUAL(par->type,"DOUB"))
         sprintf (value, "%g", par->val.d[i]);

      sprintf (msg, "%s%s", msg, value);
      l_message (msg);

      memset (msg,'\0',sizeof(msg));
      sprintf (msg,"              ");
   }

}


/*
 * use vicar call to open image files
 */
void l_open_file (fil, action)
 struct gfile *fil;
 char *action;
{
  int stat;

  if (EQUAL(fil->pdf_name,"INP") || EQUAL(fil->pdf_name,"OUT"))
     stat = zvunit (&fil->unit,fil->pdf_name,fil->instance,"");
  else
     stat = zvunit (&fil->unit,fil->pdf_name,fil->instance,
                                        "U_NAME",fil->name,"");

  if (EQUAL(action,"READ")) {
     stat = zvopen(fil->unit, "IO_ACT","SA", "OPEN_ACT","SA", "");
     stat = zvget (fil->unit, "NL",&fil->nl, "NS",&fil->ns, "NB",&fil->nb,
                                             "FORMAT",fil->format, "");
  }
  else if (EQUAL(action,"WRITE")) {
     stat = zvopen (fil->unit, "U_NL",fil->nl, "U_NS",fil->ns,
               "U_NB",fil->nb, "IO_ACT","SA", "OPEN_ACT","SA", "OP","WRITE",
               "O_FORMAT",fil->format, "");
  }

  /*
    don't want to have to play around with bytes or halves in most programs
  */
  if (EQUAL(fil->format,"BYTE") || EQUAL(fil->format,"HALF")) {
     stat = zvclose (fil->unit, "");
     if (EQUAL(action,"READ")) {
        stat = zvopen(fil->unit, "IO_ACT","SA", "OPEN_ACT","SA",
                      "U_FORMAT","FULL","");
        stat = zvget (fil->unit, "NL",&fil->nl, "NS",&fil->ns, "NB",&fil->nb,
                      "FORMAT",fil->format, "");
     }
     else if (EQUAL(action,"WRITE")) {
        stat = zvopen (fil->unit, "U_NL",fil->nl, "U_NS",fil->ns,
               "U_NB",fil->nb, "IO_ACT","SA", "OPEN_ACT","SA", "OP","WRITE",
               "U_FORMAT","FULL","O_FORMAT",fil->format, "");
     }
  }

}


/*
 * use zvclose to close a file
 */
void l_close_file (unit)
 int unit;
{
   int stat;
   stat = zvclose (unit, "CLOS_ACT","FREE", "");
}

/*
 * use zvmessage to send a message to the user
 */
void l_message (msg)
 char *msg;
{
   zvmessage(msg, "");
}

/*
 * add a label item to a vicar file
 */
void l_add_label (unit, field, value)
 int unit;
 char *field;
 char *value;
{
   char msg[80];
   int s;

   s = zladd (unit,"HISTORY",field,value,"FORMAT","STRING","");
   if (s != 1){
      sprintf (msg, " Error adding %s label to unit %d", field, unit);
      l_message (msg, "");
   }
}


/*
 * delete a vicar label item
 */

void l_delete_label (unit, field, task)
 int unit;
 char *field;
 char *task;
{
   char msg[80];
   int s;

   s = zldel (unit,"HISTORY",field,"HIST",task,"");
   if (s != 1){
      sprintf (msg, " Error deleting %s label from unit %d", field, unit);
      l_message (msg, "");
   }
}



void l_read_line (unit, buffer, line, band)
   int unit;
   int line;
   int band;
   void  *buffer;
{
   int status;

   status = zvread (unit, buffer, "LINE",line,"BAND",band, "");
}

void l_write_line (unit, buffer, line, band)
   int unit;
   int line;
   int band;
   void *buffer;
{
   int status;

   status = zvwrit (unit, buffer, "LINE",line,"BAND",band, "");
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vicar_subs.imake

#define SUBROUTINE vicar_subs

#define MODULE_LIST vicar_subs.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
