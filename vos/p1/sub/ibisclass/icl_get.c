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

