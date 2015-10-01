$!****************************************************************************
$!
$! Build proc for MIPL module del_prop_grp
$! VPACK Version 1.9, Monday, December 07, 2009, 16:10:39
$!
$! Execute by entering:		$ @del_prop_grp
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
$ write sys$output "*** module del_prop_grp ***"
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
$ write sys$output "Invalid argument given to del_prop_grp.com file -- ", primary
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
$   if F$SEARCH("del_prop_grp.imake") .nes. ""
$   then
$      vimake del_prop_grp
$      purge del_prop_grp.bld
$   else
$      if F$SEARCH("del_prop_grp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake del_prop_grp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @del_prop_grp.bld "STD"
$   else
$      @del_prop_grp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create del_prop_grp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack del_prop_grp.com -mixed -
	-s del_prop_grp.c -
	-i del_prop_grp.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create del_prop_grp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "zvproto.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/**********************************************************************
    DEL_PROP_GRP

    Deletes Property Group  from image's label with unit number.

    unit          - unit number of file containing picture (input)  integer*4
    property_name - Name of the property group to be deleted
    instance      - Particular instance of the given property group
**********************************************************************/
void del_prop_grp(unit, property_name, instance)
     int unit;
     char* property_name;
     int instance;
{
    char buf[500];
    int status;
    int maxlen, nelem;
    char key[33] = "PROPERTY";
    char format[12];
    int element_to_delete = 1;

    /* set label pointer to this subset */
    status = zlinfo(unit,
		    "PROPERTY", "PROPERTY", 
		    format, &maxlen, &nelem,
		    "PROPERTY", property_name, 
		    "INSTANCE",instance,
		    NULL);
    do {
        if (strcmp(key, "PROPERTY") != 0) {
	    status = zldel(unit, 
			   "PROPERTY", key,
			   "PROPERTY", property_name,
			   "ELEMENT", element_to_delete,
			   NULL);

	    /*sprintf(buf,"Keyword %s deleted",key);
	    zvmessage(buf, "");
	    */
	}

	/* Increment to next keyword */
	status = zlninfo(unit,key,format,&maxlen,&nelem, NULL);

    } while ((strcmp(key,"TASK") != 0) && 
	     (strcmp(key,"PROPERTY") != 0) &&
	     (status == 1));

    /* Lastly delete the property label itself */
    status = zldel(unit, "PROPERTY", "PROPERTY", 
		   "PROPERTY", property_name,
		   "INSTANCE", instance,
		   NULL);

    sprintf(buf,"Deleted %s Property Group", property_name);
    zvmessage(buf, "");
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create del_prop_grp.imake
/* Imake file for VICAR subroutine  */

#define SUBROUTINE del_prop_grp

#define MODULE_LIST del_prop_grp.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
