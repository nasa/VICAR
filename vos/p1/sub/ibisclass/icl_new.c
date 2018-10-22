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

