$!****************************************************************************
$!
$! Build proc for MIPL module ibisclass
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:15
$!
$! Execute by entering:		$ @ibisclass
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
$ write sys$output "*** module ibisclass ***"
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
$ write sys$output "Invalid argument given to ibisclass.com file -- ", primary
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
$   if F$SEARCH("ibisclass.imake") .nes. ""
$   then
$      vimake ibisclass
$      purge ibisclass.bld
$   else
$      if F$SEARCH("ibisclass.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisclass
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisclass.bld "STD"
$   else
$      @ibisclass.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisclass.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisclass.com -mixed -
	-s icl_new.c icl_get.c icl_strings.c icl_bridge.c icl.h -
	-i ibisclass.imake -
	-t tibisclass.c tibisclass.pdf tibisclass.imake tibisclass.pdf -
	   tstibisclass.pdf tibisclass_f.f
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create icl_new.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  The ICLNewX() routines are passed a set of column-lists
 *  and column-counts for each member of the class <X>. The
 *  routine installs all of the inherited class names, if
 *  needed.
 *
 *  The return value will be either the total number of
 *  columns in the C_ROOT class, or else a negative error status.
 */

#include "ibisfile.h"  /* IBIS defines */
#include "ibiserrs.h"
#include "icl.h"       /* private includes */

/*
 *  Basic utility for creating classes,members,instances,
 *   modifying if group already exists. We actually delete
 *   and re-create existing groups to move them to the bottom of
 *   the list. This insures that the classes are listed
 *   last, and the members first.
 *
 *  This routine is "semi-private"; it should only be called from
 *  the C-interface routines for named classes, and not directly
 *  by programs. 
 *
 *  The return value is the total number of values in <groupname>
 *  or else an error status.
 */
 

int ICLNewPrimitive(ibis,cols,ncols,groupname)
int ibis;
int *cols;
int ncols;
char *groupname;
{
	int col,status;

	if (ncols < 0) return IBIS_GROUP_IS_EMPTY;
	if (ncols == 0) return 0;

        if (IBISColumnFind(ibis,ITYPE_GROUP,groupname,&col,1,1)==1)
        {
           int cols1[1024],ncol1;
           status=IBISGroupModify(ibis,ITYPE_GROUP,groupname,
                 IGROUP_INSERT, cols,ncols);
	   if (status < 0) return status;
	   
           ncol1=IBISColumnFind(ibis,ITYPE_GROUP,groupname,0,0,0);
           IBISColumnFind(ibis,ITYPE_GROUP,groupname,cols1,1,ncol1);
           IBISGroupDelete(ibis,ITYPE_GROUP,groupname);
           status=IBISGroupNew(ibis,ITYPE_GROUP,groupname,cols1,ncol1,0);
	}
	else
	   status=IBISGroupNew(ibis,ITYPE_GROUP,groupname,
	          cols,ncols,0);
	
	return status;
}

/*********************************************************************
 *  Start of Class Hierarchy
 *    All of these routines have the same structure
 *    and disclipline for instantiating classes:
 *
 *      1. Create MEMBERS and gather up a list and count of cols.
 *         If the member belongs to a subclass of the one inherited
 *         the call should be made now.
 *      2. If "inst" is a non-null string, The client is instantiating
 *         this object, so create an instance group from 'inst'
 *      3. Pass control to Parent Class Routine,  with CLASS
 *         name as the instance.
 *********************************************************************/


/*
 *  All Classes are derived from C_ROOT:
 */

int ICLNewROOT(ibis,cols,ncols,inst)
int ibis;
int *cols;
int ncols;
char *inst;
{
	int status;
	if (inst)
	{
		status = ICLNewPrimitive(ibis,cols,ncols,inst);
		if (status < 0) return status;
	}
	status = ICLNewPrimitive(ibis,cols,ncols,"C_ROOT");
	return status;
}

int ICLNewPOSITION(ibis,cols,ncols,inst)
int ibis;
int *cols;
int ncols;
char *inst;
{
	int status;
	if (inst) 
	{
		status = ICLNewPrimitive(ibis,cols,ncols,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,cols,ncols,"C_POSITION");
	return status;
}

int ICLNewPOS_IMAGE(ibis,line,samp,band,inst)
int ibis;
int line;
int samp;
int band;
char *inst;
{
	int ncols=0,cols[3];
	int status;
	
	if (line) 
	{ 
		cols[ncols++] = line;
		ICLNewPrimitive(ibis,&line,1,"LINE");
	}
	if (samp) 
	{ 
		cols[ncols++] = samp;
		ICLNewPrimitive(ibis,&samp,1,"SAMP");
	}
	if (band) 
	{ 
		cols[ncols++] = band;
		ICLNewPrimitive(ibis,&band,1,"BAND");
	}
	if (inst)
	{
		status = ICLNewPrimitive(ibis,cols,ncols,inst);
		if (status < 0) return status;
	}
	status = ICLNewPOSITION(ibis,cols,ncols,"C_POS_IMAGE");
	return status;
}

int ICLNewPOS_GEOGRAPHIC(ibis,lat,longv,inst)
int ibis;
int lat;
int longv;
char *inst;
{
	int ncols=0,cols[2];
	int status;
	
	if (lat) 
	{ 
		cols[ncols++] = lat;
		ICLNewPrimitive(ibis,&lat,1,"LATITUDE");
	}
	if (longv) 
	{ 
		cols[ncols++] = longv;
		ICLNewPrimitive(ibis,&longv,1,"LONGITUDE");
	}
	if (inst)
	{
		status = ICLNewPrimitive(ibis,cols,ncols,inst);
		if (status < 0) return status;
	}
	status = ICLNewPOSITION(ibis,cols,ncols,"C_POS_GEOGRAPHIC");
	return status;
}

int ICLNewVALUE(ibis,cols,ncols,inst)
int ibis;
int *cols;
int ncols;
char *inst;
{
	int status;
	if (inst)
	{
		status = ICLNewPrimitive(ibis,cols,ncols,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,cols,ncols,"C_VALUE");
	return status;
}

int ICLNewRGB(ibis,red,green,blue,inst)
int ibis;
int red;
int green;
int blue;
char *inst;
{
	int ncols=0,cols[3];
	int status;
	
	if (red) 
	{ 
		cols[ncols++] = red;
		ICLNewPrimitive(ibis,&red,1,"RED");
	}
	if (green) 
	{ 
		cols[ncols++] = green;
		ICLNewPrimitive(ibis,&green,1,"GREEN");
	}
	if (blue) 
	{ 
		cols[ncols++] = blue;
		ICLNewPrimitive(ibis,&blue,1,"BLUE");
	}
	if (inst)
	{
		status = ICLNewPrimitive(ibis,cols,ncols,inst);
		if (status < 0) return status;
	}
	status = ICLNewVALUE(ibis,cols,ncols,"C_RGB");
	return status;
}

/*
 *  Our first real composite Class!
 */

int ICLNewPIXEL(ibis,pos,npos,val,nval,inst)
int ibis;
int *pos;
int npos;
int *val;
int nval;
char *inst;
{
	int status;
	if (npos) 
	{ 
		ICLNewPOSITION(ibis,pos,npos,"POSITION");
	}
	if (nval) 
	{ 
		ICLNewVALUE(ibis,val,nval,"VALUE");
	}
	if (inst) 
	{
		status = ICLNewPrimitive(ibis,pos,npos,inst);
		if (status < 0) return status;
		status = ICLNewPrimitive(ibis,val,nval,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,pos,npos,"C_PIXEL");
	if (status < 0) return status;
	status = ICLNewROOT(ibis,val,nval,"C_PIXEL");
	return status;
}

int ICLNewDIRECTION(ibis,cols,ncols,inst)
int ibis;
int *cols;
int ncols;
char *inst;
{
	int status;
	if (inst)
	{
		status = ICLNewPrimitive(ibis,cols,ncols,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,cols,ncols,"C_DIRECTION");
	return status;
}


int ICLNewMATRIX(ibis,mat,nmat,ind,nind,inst)
int ibis;
int *mat;
int nmat;
int *ind;
int nind;
char *inst;
{
	int status;
	if (nmat) 
	{ 
		ICLNewPrimitive(ibis,mat,nmat,"MATRIX");
	}
	if (nind) 
	{ 
		ICLNewPrimitive(ibis,ind,nind,"INDEX");
	}
	if (inst) 
	{
		status = ICLNewPrimitive(ibis,mat,nmat,inst);
		if (status < 0) return status;
		status = ICLNewPrimitive(ibis,ind,nind,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,mat,nmat,"C_MATRIX");
	if (status < 0) return status;
	status = ICLNewROOT(ibis,ind,nind,"C_MATRIX");
	return status;
}

int ICLNewQUALITY(ibis,qual,nqual,inst)
int ibis;
int *qual;
int nqual;
char *inst;
{
	int status;

	if (inst)
	{
		status= ICLNewPrimitive(ibis,qual,nqual,inst);
		if (status < 0) return status;
	}
	ICLNewROOT(ibis,qual,nqual,"QUALITY");

	/* place in scope of its owner */
	status = ICLNewROOT(ibis,qual,nqual,"C_POINT");
	
	return status;
}


/*
 *  Another override; MATRIX in HISTOGRAM is C_VALUE.
 */

int ICLNewHISTOGRAM(ibis,mat,nmat,ind,nind,inst)
int ibis;
int *mat;
int nmat;
int *ind;
int nind;
char *inst;
{
	int status;
	if (inst) 
	{
		status = ICLNewPrimitive(ibis,mat,nmat,inst);
		if (status < 0) return status;
		status = ICLNewPrimitive(ibis,ind,nind,inst);
		if (status < 0) return status;
	}
	if (nmat) 
	{ 
		/* Override */
		ICLNewVALUE(ibis,mat,nmat,0);
	}
	status = ICLNewMATRIX(ibis,mat,nmat,ind,nind,"C_HISTOGRAM");
	return status;
}

/*********************************************************************
 *
 *  Complex Classes: The classes below have more than one or two
 *   simple members, and so we allow the interface to install the
 *   members one at a time, by member-name.
 *
 *********************************************************************/


/*
 *  Lookup Table Class.
 */

int ICLNewLOOKUP_TABLE(ibis,ps,nps,ind,nind,memname,inst)
int ibis;
int *ps;
int nps;
int *ind;
int nind;
char *memname;
char *inst;
{
	int key;
	int status;
	
	key = icl_keymatch( memname, LookMemberList );
	switch (key)
	{
	   case LK_PSEUDOCOLOR:
		ICLNewMATRIX(ibis,ps,nps,ind,nind,LOOK_PSEUDOCOLOR);
		ICLNewVALUE(ibis,ps,nps,0);  /* OVERRIDE */
		break;
	   case LK_STRETCH:
		ICLNewVALUE(ibis,ps,nps,LOOK_STRETCH);
		break;
	   default:
		ICLNewPrimitive(ibis,ps,nps,memname);
		ICLNewPrimitive(ibis,ind,nind,memname);
		break;
	}
	if (inst) 
	{
		status = ICLNewPrimitive(ibis,ps,nps,inst);
		if (status < 0) return status;
		status = ICLNewPrimitive(ibis,ind,nind,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,ps,nps,"C_LOOKUP_TABLE");
	if (status < 0) return status;
	status = ICLNewROOT(ibis,ind,nind,"C_LOOKUP_TABLE");
	return status;
}

/*
 *  Statistics Class.
 */

int ICLNewSTATISTICS(ibis,mat,nmat,ind,nind,memname,inst)
int ibis;
int *mat;
int nmat;
int *ind;
int nind;
char *memname;
char *inst;
{
	int key;
	int status;
	
	key = icl_keymatch( memname, StatMemberList );
	switch (key)
	{
	   case STAT_HIST:
		ICLNewHISTOGRAM(ibis,mat,nmat,ind,nind,STAT_HISTOGRAM);
		break;
	   case STAT_CORR:
		ICLNewMATRIX(ibis,mat,nmat,ind,nind,STAT_CORRELATION);
		break;
	   case STAT_COVAR:
		ICLNewMATRIX(ibis,mat,nmat,ind,nind,STAT_COVARIANCE);
		break;
	   default:
		ICLNewPrimitive(ibis,mat,nmat,memname);
		ICLNewPrimitive(ibis,ind,nind,memname);
		break;
	}
	if (inst) 
	{
		status = ICLNewPrimitive(ibis,mat,nmat,inst);
		if (status < 0) return status;
		status = ICLNewPrimitive(ibis,ind,nind,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,mat,nmat,"C_STATISTICS");
	if (status < 0) return status;
	status = ICLNewROOT(ibis,ind,nind,"C_STATISTICS");
	return status;
}

/*
 *  POINT class.
 */


int ICLNewPOINT(ibis,pos,npos,val,nval,memname,inst)
int ibis;
int *pos;
int npos;
int *val;
int nval;
char *memname;
char *inst;
{
	int key;
	int status;
	
	key = icl_keymatch( memname, PointMemberList );
	switch (key)
	{
	   case PT_FRAME:
		ICLNewPrimitive(ibis,pos,npos,POINT_FRAME);
		break;
	   case PT_QUALITY:
		ICLNewQUALITY(ibis,pos,npos,POINT_QUALITY);
		break;
	   case PT_PIXEL:
		ICLNewPIXEL(ibis,pos,npos,val,nval,POINT_PIXEL);
		break;
	   default:
		ICLNewPrimitive(ibis,pos,npos,memname);
		ICLNewPrimitive(ibis,val,nval,memname);
		break;
	}
	if (inst) 
	{
		status = ICLNewPrimitive(ibis,pos,npos,inst);
		if (status < 0) return status;
		status = ICLNewPrimitive(ibis,val,nval,inst);
		if (status < 0) return status;
	}
	status = ICLNewROOT(ibis,pos,npos,"C_POINT");
	if (status < 0) return status;
	status = ICLNewROOT(ibis,val,nval,"C_POINT");
	return status;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create icl_get.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  The ICLGetX() routines, instead of returning lists of
 *   columns, return LOCAL groups, which may in turn be
 *   queried for number of columns, passed directly to
 *   IBIS records, or whatever. This approach made for the
 *   most hassle-free interface and systematic code.
 */

#include "ibisfile.h"  /* IBIS defines */
#include "icl.h"       /* private includes */

/*
 *  Basic utility for installing temporary groups, 
 *    replacing old groups if they already exists.
 */
 
void ICLGetPrimitive(int ibis,char *group,char* expression)
{
	if (!group) return;
	IBISGroupDelete(ibis,ITYPE_LOCAL,group);
	IBISGroupNew(ibis,ITYPE_LOCAL,group,0,0,expression);
}

/*********************************************************************
 *  Start of Class Hierarchy
 *    All of these routines have the same structure
 *    and disclipline for instantiating classes:
 *
 *      1. Create the Parent Class'es scope within the "inst" columns
 *         by creating a temporary local group $current_classname
 *      2. Extract and define the Member groups with current scope,
 *          using the ICLGet method appropriate to that member.
 *      3. Delete the temporary parent scope group
 *
 *********************************************************************/


/*
 *  All Classes are derived from C_ROOT:
 */

void ICLGetROOT(int ibis,char* group,char* inst)
{
	ICLGetPrimitive(ibis,"$C_ROOT",inst);
	ICLGetPrimitive(ibis,group,"C_ROOT & $C_ROOT");
	IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_ROOT");
}

void ICLGetPOSITION(int ibis,char* group,char* inst)
{
	ICLGetROOT(ibis,"$C_POSITION",inst);
	ICLGetPrimitive(ibis,group,"C_POSITION & $C_POSITION");
	IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_POSITION");
}

void ICLGetPOS_IMAGE(int ibis,char* gline,char* gsamp,char* gband,char *inst)
{
   ICLGetPOSITION(ibis,"$C_POS_IMAGE",inst);
   if (gline) ICLGetPrimitive(ibis,gline,"C_POS_IMAGE & $C_POS_IMAGE & LINE");
   if (gsamp) ICLGetPrimitive(ibis,gsamp,"C_POS_IMAGE & $C_POS_IMAGE & SAMP");
   if (gband) ICLGetPrimitive(ibis,gband,"C_POS_IMAGE & $C_POS_IMAGE & BAND");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_POS_IMAGE");
}

void ICLGetPOS_GEOGRAPHIC(int ibis,char* glat,char* glong,char* inst)
{
   ICLGetPOSITION(ibis,"$C_POS_GEOGRAPHIC",inst);
   if (glat)
     ICLGetPrimitive(ibis,glat, 
          "C_POS_GEOGRAPHIC & $C_POS_GEOGRAPHIC & LATITUDE");
   if (glong)
      ICLGetPrimitive(ibis,glong,
           "C_POS_GEOGRAPHIC & $C_POS_GEOGRAPHIC & LONGITUDE");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_POS_GEOGRAPHIC");
}

void ICLGetVALUE(int ibis,char* group,char* inst)
{
   ICLGetROOT(ibis,"$C_VALUE",inst);
   ICLGetPrimitive(ibis,group,"C_VALUE & $C_VALUE");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_VALUE");
}


void ICLGetRGB(int ibis,char* gred,char* ggreen,char* gblue,char* inst)
{
   ICLGetVALUE(ibis,"$C_RGB",inst);
   if (gred) ICLGetPrimitive(ibis,gred,  "C_RGB & $C_RGB & RED");
   if (ggreen) ICLGetPrimitive(ibis,ggreen,"C_RGB & $C_RGB & GREEN");
   if (gblue) ICLGetPrimitive(ibis,gblue, "C_RGB & $C_RGB & BLUE");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_RGB");
}

/*
 *  Our first real composite Class!
 */

void ICLGetPIXEL(int ibis,char* gpos,char* gval,char* inst)
{
   ICLGetROOT(ibis,"$C_PIXEL",inst);
   if (gpos)
      ICLGetPOSITION(ibis,gpos,  "C_PIXEL & $C_PIXEL & POSITION");
   if (gval)
      ICLGetVALUE(ibis,gval,"C_PIXEL & $C_PIXEL & VALUE");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_PIXEL");
}

void ICLGetDIRECTION(int ibis,char* group,char* inst)
{
   ICLGetROOT(ibis,"$C_DIRECTION",inst);
   ICLGetPrimitive(ibis,group, "C_DIRECTION & $C_DIRECTION");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_DIRECTION");
}


void ICLGetMATRIX(int ibis,char* gmat,char* gind,char* inst)
{
   ICLGetROOT(ibis,"$C_MATRIX",inst);
   if (gmat)
      ICLGetPrimitive(ibis,gmat, "C_MATRIX & $C_MATRIX & MATRIX");
   if (gind)
      ICLGetPrimitive(ibis,gind, "C_MATRIX & $C_MATRIX & INDEX");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_MATRIX");
}

void ICLGetQUALITY(int ibis,char* gqual,char* inst)
{
   ICLGetROOT(ibis,"$_QUALITY",inst);
   /* We need to use the owner's scope (C_POINT) */
   ICLGetPrimitive(ibis,gqual, "C_POINT & QUALITY & $_QUALITY");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$_QUALITY");
}


/*
 *  Another override; MATRIX in HISTOGRAM is C_VALUE.
 */

void ICLGetHISTOGRAM(int ibis,char* gmat,char* gind,char* inst)
{
   ICLGetMATRIX(ibis,"$C_HISTOGRAM_MAT","$C_HISTOGRAM_IND", inst);
   if (gmat)
     ICLGetPrimitive(ibis,gmat, "C_HISTOGRAM & $C_HISTOGRAM_MAT & POSITION");
   if (gind)
     ICLGetVALUE(ibis,gind,"C_HISTOGRAM & $C_HISTOGRAM_IND & VALUE");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_HISTOGRAM_MAT");
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_HISTOGRAM_IND");
}

/*********************************************************************
 *
 *  Complex Classes: The classes below have more than one or two
 *   simple members, and so we allow the interface to install the
 *   members one at a time, by member-name. This of course required
 *   a few utility routines for doing keyword lookup.
 *
 *********************************************************************/


/*
 *  Lookup Table Class. 
 */

void ICLGetLOOKUP_TABLE(int ibis,char* gps,char* gind,char* memname,char* inst)
{
   int key;
   
   ICLGetROOT(ibis,"$C_LOOKUP_TABLE",inst);
   key = icl_keymatch( memname, LookMemberList );
   switch (key)
   {
	case LK_PSEUDOCOLOR:
	   ICLGetMATRIX(ibis,"$C__LOOK_PS",gind,
	   	"C_LOOKUP_TABLE & $C_LOOKUP_TABLE & PSEUDOCOLOR");
	   ICLGetVALUE(ibis,gps,"$C__LOOK_PS"); /* override */
   	   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C__LOOK_PS");
	   break;
	case LK_STRETCH:
	   ICLGetVALUE(ibis,gps,
	   	"C_LOOKUP_TABLE & $C_LOOKUP_TABLE & STRETCH");
	   break;
	default:
   	   ICLGetROOT(ibis,"$C__LOOK_MEMBER",memname);
	   ICLGetVALUE(ibis,gps,
	   	"C_LOOKUP_TABLE & $C_LOOKUP_TABLE & $C__LOOK_MEMBER");
   	   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C__LOOK_MEMBER");
	break;
   }
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_LOOKUP_TABLE");
}

/*
 *  Statistics Class.
 */

void ICLGetSTATISTICS(int ibis,char* gmat,char* gind,char* memname,char *inst)
{
   int key;
   
   ICLGetROOT(ibis,"$C_STATISTICS",inst);
   key = icl_keymatch( memname, StatMemberList );
   switch (key)
   {
	case STAT_HIST:
	   ICLGetHISTOGRAM(ibis,gmat,gind,
	   	"C_STATISTICS & $C_STATISTICS & HISTOGRAM");
	   break;
	case STAT_CORR:
	   ICLGetMATRIX(ibis,gmat,gind,
	   	"C_STATISTICS & $C_STATISTICS & CORRELATION");
	   break;
	case STAT_COVAR:
	   ICLGetMATRIX(ibis,gmat,gind,
	   	"C_STATISTICS & $C_STATISTICS & COVARIANCE");
	   break;
	default:
   	   ICLGetROOT(ibis,"$C__STAT_MEMBER",memname);
	   ICLGetPrimitive(ibis,gmat,
	   	"C_STATISTICS & $C_STATISTICS & $C__STAT_MEMBER");
   	   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C__STAT_MEMBER");
	break;
   }
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_STATISTICS");
}



/*
 *  POINT class.
 */


void ICLGetPOINT(int ibis,char* gpos,char* gval,char* memname,char* inst)
{
   int key;
   
   ICLGetROOT(ibis,"$C_POINT",inst);
   key = icl_keymatch( memname, PointMemberList );
   switch (key)
   {
	case PT_FRAME:
	   ICLGetPrimitive(ibis,gpos,"C_POINT & $C_POINT & FRAME");
	   break;
	case PT_QUALITY:
	   ICLGetQUALITY(ibis,gpos,"C_POINT & $C_POINT & QUALITY");
	   break;
	case PT_PIXEL:
	   ICLGetPIXEL(ibis,gpos,gval,"C_POINT & $C_POINT & PIXEL");
	   break;
	default:
   	   ICLGetROOT(ibis,"$C__PT_MEMBER",memname);
	   ICLGetPrimitive(ibis,gpos,
	   	"C_POINT & $C_POINT & $C__PT_MEMBER");
   	   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C__PT_MEMBER");
	break;
   }
   IBISGroupDelete(ibis,ITYPE_LOCAL,"$C_POINT");
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create icl_strings.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  ICL keyword search utilities (case-insensitive)
 */

#include <string.h>
#include <ctype.h>


static int strcmp_nocase(str1,str2)
char *str1;
char *str2;
{
	char c1,c2;
	
	for((c1= *str1,c2= *str2); c1&&c2; (c1= *++str1,c2= *++str2) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return (c1 || c2);
}


int icl_keymatch(char *keystr, char **keys)
/* keystr: input key string; keys: array of pointers to keys */
{
	int keyno;
	
	if (!keystr) return 0;
	for (keyno=1; *keys && strcmp_nocase(*keys,keystr); keys++)
		keyno++;
	
	if (*keys) return (keyno);
	else return 0;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create icl_bridge.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*********************************************************************
 *  FORTRAN Bridges for ICL routines
 *
 *  Don't even bother looking here; there is nothing but
 *  symantic sugar for the icl_new and icl_get modules.
 *
 *********************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include "ibisfile.h"

#define MAX_GRP_SIZE 32

void FTN_NAME2_(icl_get_root, ICL_GET_ROOT) (int *ibis,char *group,
		char *inst, ZFORSTR_PARAM)
#if 0
char *group;  /* name of group */
#endif
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetROOT(*ibis,grp,in);
}

void FTN_NAME2_(icl_get_position, ICL_GET_POSITION) (int *ibis, char *group,
		char *inst,ZFORSTR_PARAM)
#if 0
char *group;  /* name of group */
#endif
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPOSITION(*ibis,grp,in);
}

void FTN_NAME2_(icl_get_pos_image, ICL_GET_POS_IMAGE) (int *ibis, char *gline,
		char *gsamp, char *gband, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char gline_c[MAX_GRP_SIZE+1],*gl=(char *)0;
	char gsamp_c[MAX_GRP_SIZE+1],*gs=(char *)0;
	char gband_c[MAX_GRP_SIZE+1],*gb=(char *)0;
	
	zsfor2c(gline_c,MAX_GRP_SIZE,gline,&ibis,5,2,1, inst);
	if (strlen(gline_c)) gl=gline_c;
	zsfor2c(gsamp_c,MAX_GRP_SIZE,gsamp,&ibis,5,3,2, inst);
	if (strlen(gsamp_c)) gs=gsamp_c;
	zsfor2c(gband_c,MAX_GRP_SIZE,gband,&ibis,5,4,3, inst);
	if (strlen(gband_c)) gb=gband_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPOS_IMAGE(*ibis,gl,gs,gb,in);
}

void FTN_NAME2_(icl_get_pos_geographic, ICL_GET_POS_GEOGRAPHIC) (int *ibis,
		char *glat, char *glong, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char glat_c[MAX_GRP_SIZE+1],*gla=(char *)0;
	char glong_c[MAX_GRP_SIZE+1],*glo=(char *)0;
	
	zsfor2c(glat_c,MAX_GRP_SIZE,glat,&ibis,4,2,1, inst);
	if (strlen(glat_c)) gla=glat_c;
	zsfor2c(glong_c,MAX_GRP_SIZE,glong,&ibis,4,3,2, inst);
	if (strlen(glong_c)) glo=glong_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
		
	ICLGetPOS_GEOGRAPHIC(*ibis,gla,glo,in);
}

void FTN_NAME2_(icl_get_value, ICL_GET_VALUE) (int *ibis, char *group,
		char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetVALUE(*ibis,grp,in);
}


void FTN_NAME2_(icl_get_rgb, ICL_GET_RGB) (int *ibis, char *gred,
		char *ggreen, char *gblue, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gred,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,ggreen,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,gblue,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetRGB(*ibis,gg1,gg2,gg3,in);
}

void FTN_NAME2_(icl_get_pixel, ICL_GET_PIXEL) (int *ibis, char *gpos,
		char *gval, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gpos,&ibis,4,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gval,&ibis,4,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPIXEL(*ibis,gg1,gg2,in);
}

void FTN_NAME2_(icl_get_direction, ICL_GET_DIRECTION) (int *ibis, char *group,
		char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetDIRECTION(*ibis,grp,in);
}


void FTN_NAME2_(icl_get_matrix, ICL_GET_MATRIX) (int *ibis, char *gmat,
		char *gind, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gmat,&ibis,4,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,4,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetMATRIX(*ibis,gg1,gg2,in);
}

void FTN_NAME2_(icl_get_quality, ICL_GET_QUALITY) (int *ibis, char *group,
		char *inst,ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,group,&ibis,3,2,1, inst);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,3,3,2, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetQUALITY(*ibis,grp,in);
}

void FTN_NAME2_(icl_get_histogram, ICL_GET_HISTOGRAM) (int *ibis, char *gmat,
		char *gind, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gmat,&ibis,4,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,4,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,4,4,3, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetHISTOGRAM(*ibis,gg1,gg2,in);
}

void FTN_NAME2_(icl_get_lookup_table, ICL_GET_LOOKUP_TABLE) (int *ibis,
		char *gps, char *gind, char *memname, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gps,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,memname,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetLOOKUP_TABLE(*ibis,gg1,gg2,gg3,in); /* gg3 is actually a member */
}

void FTN_NAME2_(icl_get_statistics, ICL_GET_STATISTICS) (int *ibis, char *gmat,
		char *gind, char *memname, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gmat,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gind,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,memname,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetSTATISTICS(*ibis,gg1,gg2,gg3,in); /* gg3 is actually a member */
}

void FTN_NAME2_(icl_get_point, ICL_GET_POINT) (int *ibis, char *gpos,
		char *gval, char *memname, char *inst, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char ggrp1_c[MAX_GRP_SIZE+1],*gg1=(char *)0;
	char ggrp2_c[MAX_GRP_SIZE+1],*gg2=(char *)0;
	char ggrp3_c[MAX_GRP_SIZE+1],*gg3=(char *)0;
	
	zsfor2c(ggrp1_c,MAX_GRP_SIZE,gpos,&ibis,5,2,1, inst);
	if (strlen(ggrp1_c)) gg1=ggrp1_c;
	zsfor2c(ggrp2_c,MAX_GRP_SIZE,gval,&ibis,5,3,2, inst);
	if (strlen(ggrp2_c)) gg2=ggrp2_c;
	zsfor2c(ggrp3_c,MAX_GRP_SIZE,memname,&ibis,5,4,3, inst);
	if (strlen(ggrp3_c)) gg3=ggrp3_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,5,4, inst);
	if (strlen(inst_c)) in=inst_c;
	
	ICLGetPOINT(*ibis,gg1,gg2,gg3,in); /* gg3 is actually a member */
}


/*********************************************************************
 *  FORTRAN Bridges for ICLNew
 *********************************************************************/


void FTN_NAME2_(icl_new_root, ICL_NEW_ROOT) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status = ICLNewROOT(*ibis,cols,*ncols,in);
}

void FTN_NAME2_(icl_new_position, ICL_NEW_POSITION) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOSITION(*ibis,cols,*ncols,in);
}

void FTN_NAME2_(icl_new_pos_image, ICL_NEW_POS_IMAGE) (int *ibis, int *line,
		int *samp, int *band, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,6,5,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOS_IMAGE(*ibis,*line,*samp,*band,in);
}

void FTN_NAME2_(icl_new_pos_geographic, ICL_NEW_POS_GEOGRAPHIC) (int *ibis,
		int *lat, int *longv, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOS_GEOGRAPHIC(*ibis,*lat,*longv,in);
}

void FTN_NAME2_(icl_new_value, ICL_NEW_VALUE) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewVALUE(*ibis,cols,*ncols,in);
}

void FTN_NAME2_(icl_new_rgb, ICL_NEW_RGB) (int *ibis,
		int *red, int *green, int *blue,
		char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,6,5,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewRGB(*ibis,*red,*green,*blue,in);
}

void FTN_NAME2_(icl_new_pixel, ICL_NEW_PIXEL) (int *ibis, int *pos, int *npos,
		int *val, int *nval, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,7,6,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPIXEL(*ibis,pos,*npos,val,*nval,in);
}

void FTN_NAME2_(icl_new_direction, ICL_NEW_DIRECTION) (int *ibis, int *cols,
		int *ncols, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewDIRECTION(*ibis,cols,*ncols,in);
}


void FTN_NAME2_(icl_new_matrix, ICL_NEW_MATRIX) (int *ibis, int *mat,
		int *nmat, int *ind, int *nind, char *inst,
		int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,7,6,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewMATRIX(*ibis,mat,*nmat,ind,*nind,in);
}

void FTN_NAME2_(icl_new_quality, ICL_NEW_QUALITY) (int *ibis, int *qual,
		int *nqual, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,5,4,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewQUALITY(*ibis,qual,*nqual,in);
}

void FTN_NAME2_(icl_new_histogram, ICL_NEW_HISTOGRAM) (int *ibis, int *mat,
	int *nmat, int *ind, int *nind, char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,7,6,1, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewHISTOGRAM(*ibis,mat,*nmat,ind,*nind,in);
}


void FTN_NAME2_(icl_new_lookup_table, ICL_NEW_LOOKUP_TABLE) (int *ibis,
		int *ps, int *nps, int *ind, int *nind, char *memname,
		char *inst, int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,memname,&ibis,8,6,1, status);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,8,7,2, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewLOOKUP_TABLE(*ibis,ps,*nps,ind,*nind,grp,in);
}

void FTN_NAME2_(icl_new_statistics, ICL_NEW_STATISTICS) (int *ibis, int *mat,
		int *nmat, int *ind, int *nind, char *memname, char *inst,
		int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,memname,&ibis,8,6,1, status);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,8,7,2, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewSTATISTICS(*ibis,mat,*nmat,ind,*nind,grp,in);
}


void FTN_NAME2_(icl_new_point, ICL_NEW_POINT) (int *ibis, int *pos, int *npos,
		int *val, int *nval, char *memname, char *inst,
		int *status, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
	char inst_c[MAX_GRP_SIZE+1],*in=(char *)0;
	char group_c[MAX_GRP_SIZE+1],*grp=(char *)0;
	
	zsfor2c(group_c,MAX_GRP_SIZE,memname,&ibis,8,6,1, status);
	if (strlen(group_c)) grp=group_c;
	zsfor2c(inst_c,MAX_GRP_SIZE,inst,&ibis,8,7,2, status);
	if (strlen(inst_c)) in=inst_c;
	
	*status =ICLNewPOINT(*ibis,pos,*npos,val,*nval,grp,in);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create icl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  Common stuff for ICL utilities (private interface)
 */

#ifndef _H_ICL
#define _H_ICL


/*
 *  List of LOOKUP members that we recognize.
 */

#define LOOK_STRETCH "HISTOGRAM"
#define LOOK_PSEUDOCOLOR "PSEUDOCOLOR"

static char *LookMemberList[]={
	LOOK_STRETCH,
	LOOK_PSEUDOCOLOR,
	(char *)0
};
typedef enum {
	LK_STRETCH=1,
	LK_PSEUDOCOLOR
} look_type;


/*
 *  List of STATISTICS members that we recognize.
 */

#define STAT_HISTOGRAM "HISTOGRAM"
#define STAT_CORRELATION "CORRELATION"
#define STAT_COVARIANCE "COVARIANCE"

static char *StatMemberList[]={
	STAT_HISTOGRAM,
	STAT_CORRELATION,
	STAT_COVARIANCE,
	(char *)0
};
typedef enum {
	STAT_HIST=1,
	STAT_CORR,
	STAT_COVAR
} stat_type;

/*
 *  List of POINT members that we recognize.
 */

#define POINT_FRAME "FRAME"
#define POINT_QUALITY "QUALITY"
#define POINT_PIXEL "PIXEL"

static char *PointMemberList[]={
	POINT_FRAME,
	POINT_QUALITY,
	POINT_PIXEL,
	(char *)0
};
typedef enum {
	PT_FRAME=1,
	PT_QUALITY,
	PT_PIXEL
} point_type;


int icl_keymatch(char *keystr, char **keys);

#endif /* ICL */

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibisclass.imake
#define SUBROUTINE ibisclass

#define P1_SUBLIB
#define USES_ANSI_C
#if !AXP_UNIX_ARCH	/* crashes the compiler!?! */
#define FTN_STRING
#endif

#define LIB_RTL

#define MODULE_LIST icl_get.c icl_new.c icl_bridge.c icl_strings.c
#define INCLUDE_LIST icl.h


$ Return
$!#############################################################################
$Test_File:
$ create tibisclass.c
#include "vicmain_c"
#include "ibisfile.h"
#include "ftnbridge.h"
#include <ctype.h>
 
main44()
{
    int count,def;
    char command[20];
    
    zvparm("_SUBCMD",command,&count,&def,0,0);
    switch (tolower(command[0]))
    {
        case 'n' : new_class(); break;
        case 'g' : get_class(); break;
        case 'f' : switch(tolower(command[1]))
	{
		/* call FORTRAN from C */
		case 'n' : FTN_NAME(new_class_fortran)(); break;
		case 'g' : FTN_NAME(get_class_fortran)(); break;
	}
    }
}

 
new_class()
{
      int unit, i, ibis,status;
      int lutcols[3];
      int pixpt[3];
      int valpt[1];
      int frame[1];
      char *user_instance="myinst";

      for (i=0;i<3;i++) lutcols[i]=i+1;
      for (i=0;i<3;i++) pixpt[i]=i+4;
      valpt[0]=7; frame[0]=8;


      /* create IBIS File */
      status = zvunit(&unit,"out",1,0);
      status = IBISFileOpen(unit,&ibis,IMODE_WRITE,
          10,256,0,IORG_COLUMN);
      if (status!=1) IBISSignalU(unit,status,1); /* abort */

      /* 
       * Set up groups. If this were a pre-existing file
       *  we would have to check to see if the groups
       *  already existed, in which case we would use
       *  IBISGroupModify( "APPEND") to add the columns.
       */

      /*
       *  The stretch is by default only in the class C_VALUE.
       *   We need to override this so that it will be RGB.
       *   No instance needed, so we pass in 0.
       */
       
      status = ICLNewRGB(ibis,lutcols[0],lutcols[1],lutcols[2],0);
      if (status < 0) IBISSignal(ibis,status,0);
      status = ICLNewLOOKUP_TABLE(ibis,lutcols,3,0,0,"PSEUDOCOLOR","MyLut");
      if (status < 0) IBISSignal(ibis,status,0);


      /*
       *  Declare value and pixel subtypes for POINT
       */
      status = ICLNewVALUE(ibis,valpt,1,"DN");
      if (status < 0) IBISSignal(ibis,status,0);
      status = ICLNewPOS_IMAGE(ibis,pixpt[0],pixpt[1],pixpt[2],0);
      if (status < 0) IBISSignal(ibis,status,0);

      /* create the point class */
      status = ICLNewPOINT(ibis,pixpt,3,valpt,1,"PIXEL","MyPOINT");
      if (status < 0) IBISSignal(ibis,status,0);
      status = ICLNewPOINT(ibis,frame,1,0,0,"FRAME","MyPOINT");
      if (status < 0) IBISSignal(ibis,status,0);

      status = IBISFileClose(ibis,0);

}


get_class()
{
      int unit, ibis,status,count;
      int lutcols[3];
      int pixpt[3];
      int valpt[1];
      int frame[1];
      char message[80];

      /* create IBIS File */
      status = zvunit(&unit,"inp",1,0);
      status = IBISFileOpen(unit,&ibis,IMODE_READ,0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1); /* abort */

      /* 
       * Get groups. We go exactly backwards from the
       *  order in the "new" routine.
       */

      /*
       *  The stretch was by default only in the class C_VALUE.
       *   We needed to override this so that it would be RGB.
       *   No index needed, so we pass in 0.
       */
       
      ICLGetLOOKUP_TABLE(ibis,"$MyLut",0,"PSEUDOCOLOR","MyLut");
      ICLGetRGB(ibis,"$MyRED","$MyGRN","$MyBLU","$MyLut");

      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyRED",lutcols,1,3);
      sprintf(message,"%d RED LUT Cols: %d...",count,lutcols[0]);
      zvmessage(message," ");
      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyGRN",lutcols,1,3);
      sprintf(message,"%d GRN LUT Cols: %d...",count,lutcols[0]);
      zvmessage(message," ");
      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyBLU",lutcols,1,3);
      sprintf(message,"%d BLUE LUT Cols: %d...",count,lutcols[0]);
      zvmessage(message," ");

      /* Get the point class members */
      ICLGetPOINT(ibis,"$MyFrame",0,"FRAME","MyPOINT");
      ICLGetPOINT(ibis,"$MyPOS","$MyVal","PIXEL","MyPOINT");

      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyFrame",frame,1,1);
      sprintf(message,"%d FRAME Cols: %d...",count,frame[0]);
      zvmessage(message," ");
      
      /*
       *  Get value and pixel subtypes from POINT
       */
      ICLGetVALUE(ibis,"$MyDN","$MyVal & DN");
      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyDN",valpt,1,1);
      sprintf(message,"%d DN Cols: %d...",count,valpt[0]);
      zvmessage(message," ");

      ICLGetPOS_IMAGE(ibis,"$MyLINE","$MySAMP","$MyBAND","$MyPOS");
      count = IBISColumnFind(ibis,ITYPE_ANY,
      		"$MyLINE | $MySAMP | $MyBAND",pixpt,1,3);
      sprintf(message,"%d POSITION Cols:(L,S,B)= %d,%d,%d",count,
      		pixpt[0],pixpt[1],pixpt[2]);
      zvmessage(message," ");

      status = IBISFileClose(ibis,0);

}
$!-----------------------------------------------------------------------------
$ create tibisclass.pdf
process

subcmd new
	parm out string count=1
end-sub

subcmd get
	parm inp string count=1
end-sub

subcmd fnew
	parm out string count=1
end-sub

subcmd fget
	parm inp string count=1
end-sub

end-proc
$!-----------------------------------------------------------------------------
$ create tibisclass.imake
#define PROGRAM tibisclass

#define TEST

#define USES_C
#define USES_FORTRAN
#define MAIN_LANG_FORTRAN
#define FTN_STRING

#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB

#define MODULE_LIST tibisclass.c tibisclass_f.f

$!-----------------------------------------------------------------------------
$ create tibisclass.pdf
process

subcmd new
	parm out string count=1
end-sub

subcmd get
	parm inp string count=1
end-sub

subcmd fnew
	parm out string count=1
end-sub

subcmd fget
	parm inp string count=1
end-sub

end-proc
$!-----------------------------------------------------------------------------
$ create tstibisclass.pdf
procedure
refgbl $autousage
body
 let $autousage="none"
 ! Test C interface
 tibisclass-new testfile
 ibis-list testfile 'groups nr=5
 tibisclass-get testfile
 ! Test FORTRAN interface
 tibisclass-fnew testfile
 ibis-list testfile 'groups nr=5
 tibisclass-fget testfile
end-proc
$!-----------------------------------------------------------------------------
$ create tibisclass_f.f

      subroutine new_class_fortran
      integer unit, i, ibis,status
      integer lutcols(3)
      integer pixpt(3)
      integer valpt
      integer frame
      
      do i=1,3
         lutcols(i)=i
         pixpt(i)=i+3
      enddo
      valpt=7
      frame=8
      
      
      ! create IBIS File 
      call xvunit(unit,'out',1,status,' ')
      call ibis_file_open(unit,ibis,'write',
     +         10,256,' ',' ',status)
      if (status.ne.1) call ibis_signal_u(unit,status,1)
      
      ! 
      ! Set up groups. If this were a pre-existing file
      !  we would have to check to see if the groups
      !  already existed, in which case we would use
      !  IBISGroupModify( 'APPEND') to add the columns.
      
      
      !
      !  The stretch is by default only in the class C_VALUE.
      !   We need to override this so that it will be RGB.
      !   No instance needed, so we pass in a blank.
      
      
      call icl_new_rgb(ibis,lutcols(1),lutcols(2),
     +                      lutcols(3),' ',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      call icl_new_lookup_table(ibis,lutcols,3,0,0,
     +                     'PSEUDOCOLOR','MyLut',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      
      
      !
      !  Declare value and pixel subtypes for POINT
      
      call icl_new_value(ibis,valpt,1,'DN',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      call icl_new_pos_image(ibis,pixpt(1),pixpt(2),
     +                            pixpt(3),' ',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      
      ! create the pointer class 
      call icl_new_point(ibis,pixpt,3,valpt,1,
     +           'PIXEL','MyPOINT',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      call icl_new_point(ibis,frame,1,0,0,'FRAME','MyPOINT',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      
      call ibis_file_close(ibis,' ',status)
      
      return
      end
      
      
      
      subroutine get_class_fortran
      integer unit, ibis,status,count
      integer lutcols(3)
      integer pixpt(3)
      integer valpt
      integer frame
      integer ibis_column_find
      character*80 message
      
      ! open IBIS File 
      call xvunit(unit,'inp',1,status,' ')
      call ibis_file_open(unit,ibis,'read',0,0,' ',' ',status)
      if (status.ne.1) call ibis_signal_u(unit,status,1)  

      ! 
      ! Get groups. We go exactly backwards from the
      !  order in the 'new' routine.
      !
      
      !
      !  The stretch was by default only in the class C_VALUE.
      !   We needed to override this so that it would be RGB.
      !   No index needed, so we pass in blank. Note that the
      !   'get' routines do not have a status; we detect failure
      !   by counting the columns in the local group we define.
      !
       
      call icl_get_lookup_table(ibis,'$MyLut',
     +   ' ','PSEUDOCOLOR','MyLut')
      call icl_get_rgb(ibis,'$MyRED','$MyGRN','$MyBLU','$MyLut')

      count = ibis_column_find(ibis,'LOCAL','$MyRED',lutcols,1,3)
      write(message,'(I2,A,I2)') count,' RED LUT Cols: ',lutcols(1)
      call xvmessage(message,' ')
      count = ibis_column_find(ibis,'LOCAL','$MyGRN',lutcols,1,3)
      write(message,'(I2,A,I2)') count,' GRN LUT Cols: ',lutcols(1)
      call xvmessage(message,' ')
      count = ibis_column_find(ibis,'LOCAL','$MyBLU',lutcols,1,3)
      write(message,'(I2,A,I2)') count,' BLUE LUT Cols: ',lutcols(1)
      call xvmessage(message,' ')

      ! Get the FRAME and PIXEL members of POINT instance 'MyPOINT'
      call icl_get_point(ibis,'$MyFrame',' ','FRAME','MyPOINT')
      call icl_get_point(ibis,'$MyPOS','$MyVal','PIXEL','MyPOINT')

      count = ibis_column_find(ibis,'LOCAL','$MyFrame',frame,1,1)
      write(message,'(I2,A,I2)') count,' FRAME LUT Cols: ',frame
      call xvmessage(message,' ')
      
      !
      !  Get value and pixel subtypes from POINT
      !
      call icl_get_value(ibis,'$MyDN','$MyVal & DN')
      count = ibis_column_find(ibis,'LOCAL','$MyDN',valpt,1,1)
      write(message,'(I2,A,I2)') count,' DN LUT Cols: ',valpt
      call xvmessage(message,' ')

      call icl_get_pos_image(ibis,'$MyLINE',
     +             '$MySAMP','$MyBAND','$MyPOS')
      count = ibis_column_find(ibis,'ANY',
     +             '$MyLINE | $MySAMP | $MyBAND',pixpt,1,3)
      write(message,'(I2,A,3I2)') 
     +   count,' POSITION Cols:(L,S,B)= ',pixpt(1),pixpt(2),pixpt(3)
      call xvmessage(message,' ')

      call ibis_file_close(ibis,' ',status)
      
      return
      end
      
$ Return
$!#############################################################################
