/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_nav_site_transform.h"

/******************************************************************************
 *				_LBL_NAV_SITE_TRANSFORM
 *
 *	This module contains routines to help create, read/write and print
 *  a MINI_HEADE Rroperty label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  package basically defines a table that the lower-level routines use.
 *  The table is the bridge between how the application access the label
 *  elements, and how the label processor specifies the label components
 *  to the VICAR label Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_command.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblNavSiteTransformHeader.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblNavSiteTransformHeader_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {

    {"SITE_FRAME_NORTH",        "REAL",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblNavSiteTransformHeader_typ, SiteFrameNorth.Value),
        LBL_OFFSET(LblNavSiteTransformHeader_typ, SiteFrameNorth.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SiteFrameNorth.Value)},

    {"SITE_FRAME_EAST",        "REAL",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblNavSiteTransformHeader_typ, SiteFrameEast.Value),
        LBL_OFFSET(LblNavSiteTransformHeader_typ, SiteFrameEast.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SiteFrameEast.Value)},

    {"SITE_FRAME_DOWN",        "REAL",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblNavSiteTransformHeader_typ, SiteFrameDown.Value),
        LBL_OFFSET(LblNavSiteTransformHeader_typ, SiteFrameDown.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SiteFrameDown.Value)},

    {"CURRENT_NAV_X",        "REAL",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblNavSiteTransformHeader_typ, CurrentNavX.Value),
        LBL_OFFSET(LblNavSiteTransformHeader_typ, CurrentNavX.Valid),
        LBL_NO_RETURN,  LBL_SIZE(CurrentNavX.Value)},

    {"CURRENT_NAV_Y",        "REAL",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblNavSiteTransformHeader_typ, CurrentNavY.Value),
        LBL_OFFSET(LblNavSiteTransformHeader_typ, CurrentNavY.Valid),
        LBL_NO_RETURN,  LBL_SIZE(CurrentNavY.Value)},

    {"CURRENT_NAV_Z",        "REAL",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblNavSiteTransformHeader_typ, CurrentNavZ.Value),
        LBL_OFFSET(LblNavSiteTransformHeader_typ, CurrentNavZ.Valid),
        LBL_NO_RETURN,  LBL_SIZE(CurrentNavZ.Value)},

    {"DELTA_YAW",        "REAL",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblNavSiteTransformHeader_typ, DeltaYaw.Value),
        LBL_OFFSET(LblNavSiteTransformHeader_typ, DeltaYaw.Valid),
        LBL_NO_RETURN,  LBL_SIZE(DeltaYaw.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/*static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"NAV_SITE_TRANSFORM_PARMS",	LBL_NULL };
*/
static LblApiProcess_typ    Label = {
    LabelTbl,   "PROPERTY", "PROPERTY", "NAV_SITE_TRANSFORM",
    LBL_NULL };

/******************************************************************************
 *				_LBL_NAV_SITE_TRANSFORM
 *
 *****************************************************************************/
/*int     LblNavSiteTransform(
  int   Unit,
  int   Obtain,
  LblNavSiteTransform_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblNavSiteTransform_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}*/
int     LblNavSiteTransformHeader(
  int   Unit,
  int   Obtain,
  LblNavSiteTransformHeader_typ      *LabelItems,
  int   Instance,
  const char* propertyName)
{ int   RtnStatus;

 if (propertyName!=NULL)
   Label.NameValue = propertyName;

  LblApiCntrl_typ   Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblNavSiteTransformHeader_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}
/******************************************************************************
 *				LBL_PRINT_NAV_SITE_TRANSFORM
 *
 *****************************************************************************/
void	LblPrintNavSiteTransformHeader(
  LblNavSiteTransformHeader_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}
/******************************************************************************
 *				LBL_TEST_NAV_SITE_TRANSFORM
 *
 *****************************************************************************/
void	LblTestNavSiteTransformHeader(
  LblNavSiteTransformHeader_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
