$!****************************************************************************
$!
$! Build proc for MIPL module find_hist_key
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:15
$!
$! Execute by entering:		$ @find_hist_key
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
$ write sys$output "*** module find_hist_key ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to find_hist_key.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("find_hist_key.imake") .nes. ""
$   then
$      vimake find_hist_key
$      purge find_hist_key.bld
$   else
$      if F$SEARCH("find_hist_key.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake find_hist_key
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @find_hist_key.bld "STD"
$   else
$      @find_hist_key.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create find_hist_key.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack find_hist_key.com -mixed -
	-s find_hist_key.c -
	-i find_hist_key.imake -
	-t tfind_hist_key.c tfind_hist_key.pdf tfind_hist_key.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create find_hist_key.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "errdefs.h"
#include "defines.h"
#include "zvproto.h"
#include <stdlib.h>
#include <string.h>

#define TASK_NAME_LENGTH 33		/* 32 chars +1 for null term */
#define NTASKS_GUESS 20			/* enough for most labels */

/************************************************************************/
/* find_hist_key							*/
/*									*/
/* Search the history label of the given file to find either the first	*/
/* or last occurrence of the label key in the label as a whole, and	*/
/* return the task name and instance number of where it was found.	*/
/* Returns SUCCESS if the key was found, or NO_SUCH_KEY if not.  Other	*/
/* VICAR errors may be returned as well, if they occur.  Note that	*/
/* the standard error actions are turned off for this routine, so it is	*/
/* the caller's responsibility to check the return code and call	*/
/* zvsignal() if needed.						*/
/*									*/
/* Written by: Bob Deen 7/12/95						*/
/************************************************************************/

int find_hist_key
(
    int unit,		/* in: unit number */
    char *key,		/* in: key to search for */
    int lastflag,	/* in: TRUE=find last, FALSE=find first */
    char *task,		/* out: task name where key found */
    int *instance	/* out: instance number of task where key found */
)
{
   char *tasks;
   int *instances;
   int ntasks, total_tasks;
   int status;
   int iter_count;
   char format_rtn[20];			/* dummies for zlinfo */
   int maxlen_rtn, nel_rtn;		/* ditto */
   int i, cur_task;

   /* Allocate a buffer for zlhinfo that will cover most cases.  If the	*/
   /* buffer isn't big enough, we'll expand it and try again.		*/

   total_tasks = NTASKS_GUESS;
   iter_count = 0;

   do {
      if (iter_count++ > 3)	/* Check for infinite loop */
         return INTERNAL_ERROR;	/* (it should only loop twice at most) */

      ntasks = total_tasks;
      tasks = (char *)malloc(ntasks * TASK_NAME_LENGTH);
      if (tasks == NULL)
         return INSUFFICIENT_MEMORY;
      instances = (int *)malloc(ntasks * sizeof(int));
      if (instances == NULL)
         return INSUFFICIENT_MEMORY;

      status = zlhinfo(unit, tasks, instances, &ntasks, "ERR_ACT", "",
		"NRET", &total_tasks, "ULEN", TASK_NAME_LENGTH, NULL);

      if (status != SUCCESS) {
         free(tasks);
         free(instances);
         return status;
      }

      /* Check to see if we got them all */

      if (total_tasks > ntasks) {		/* nope, recycle */
         free(tasks);
         free(instances);
      }
   } while (total_tasks > ntasks);

   /* Now we have the history task list, so search it to find the label */

   for (i=0; i<ntasks; i++) {
      if (lastflag)			/* search from the end */
         cur_task = ntasks - i - 1;
      else				/* search from the start */
         cur_task = i;

      status = zlinfo(unit, "history", key, format_rtn, &maxlen_rtn, &nel_rtn,
		"ERR_ACT", "", "HIST", tasks + (cur_task*TASK_NAME_LENGTH),
		"INSTANCE", instances[cur_task], NULL);

      if (status == SUCCESS) {		/* Found it! */
         strcpy(task, tasks + (cur_task * TASK_NAME_LENGTH));
         *instance = instances[cur_task];
         free(tasks);
         free(instances);

         return SUCCESS;
      }
   }

   /* Key not found in label */

   free(tasks);
   free(instances);

   return NO_SUCH_KEY;

}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create find_hist_key.imake
#define SUBROUTINE find_hist_key
#define MODULE_LIST find_hist_key.c

#define P1_SUBLIB
#define USES_C

$ Return
$!#############################################################################
$Test_File:
$ create tfind_hist_key.c
#include "vicmain_c"

main44()
{
   int unit;
   char key[100];
   char task[100];
   int instance;
   int count;
   int status;
   char msg[256];

   zveaction("sa","");

   zvunit(&unit, "inp", 1, 0);
   zvopen(unit, "op", "read", 0);

   zvp("key", key, &count);

   /* Find first */

   status = find_hist_key(unit, key, 0, task, &instance);

   sprintf(msg, "First occurrence: status=%d, task='%s', instance=%d",
		status, task, instance);
   zvmessage(msg, "");

   /* Find last */

   status = find_hist_key(unit, key, 1, task, &instance);

   sprintf(msg, "Last occurrence: status=%d, task='%s', instance=%d",
		status, task, instance);
   zvmessage(msg, "");

   zvclose(unit, 0);

}

$!-----------------------------------------------------------------------------
$ create tfind_hist_key.pdf
process help=*
parm inp string
parm key string
end-proc

$!-----------------------------------------------------------------------------
$ create tfind_hist_key.imake
#define PROGRAM tfind_hist_key
#define MODULE_LIST tfind_hist_key.c

#define MAIN_LANG_C
#define USES_C

#define TEST

#define LIB_P1SUB
#define LIB_RTL
#define LIB_TAE

/* #define LIB_LOCAL */

$ Return
$!#############################################################################
