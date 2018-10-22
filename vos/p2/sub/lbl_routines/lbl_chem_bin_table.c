/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_chem_bin_table.h"

/******************************************************************************
 *				_LBL_CHEM_BIN_TABLE
 *
 *	This module contains routines to help create, read/write and print
 *  a ChemBinTable property label.  It is part of the MIPL label API
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
 *  LblChemBinTable.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblChemBinTable_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {

	{"PDS_OBJECT__TYPE",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, PdsObjectType.Value),
                LBL_OFFSET(LblChemBinTable_typ, PdsObjectType.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PdsObjectType.Value)},

	{"PDS_OBJECT__PTR",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, PdsObjectPtr.Value),
                LBL_OFFSET(LblChemBinTable_typ, PdsObjectPtr.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PdsObjectPtr.Value)},

	{"PDS_OBJECT__OFFSET",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, PdsObjectOffset.Value),
                LBL_OFFSET(LblChemBinTable_typ, PdsObjectOffset.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PdsObjectOffset.Value)},

	{"PDS_OBJECT__LOC",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, PdsObjectLoc.Value),
                LBL_OFFSET(LblChemBinTable_typ, PdsObjectLoc.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PdsObjectLoc.Value)},

	{"BYTES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, Bytes.Value),
                LBL_OFFSET(LblChemBinTable_typ, Bytes.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Bytes.Value)},

	{"INTERCHANGE_FORMAT",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, InterchangeFormat.Value),
                LBL_OFFSET(LblChemBinTable_typ, InterchangeFormat.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InterchangeFormat.Value)},

	{"ROWS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, Rows.Value),
                LBL_OFFSET(LblChemBinTable_typ, Rows.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Rows.Value)},

	{"COLUMNS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, Columns.Value),
                LBL_OFFSET(LblChemBinTable_typ, Columns.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Columns.Value)},

	{"ROW_BYTES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, RowBytes.Value),
                LBL_OFFSET(LblChemBinTable_typ, RowBytes.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RowBytes.Value)},

	{"DESCRIPTION",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, Description.Value),
                LBL_OFFSET(LblChemBinTable_typ, Description.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Description.Value)},

	{"STRUCTURE__PTR",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblChemBinTable_typ, StructurePtr.Value),
                LBL_OFFSET(LblChemBinTable_typ, StructurePtr.Valid),
                LBL_NO_RETURN,  LBL_SIZE(StructurePtr.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/*static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"CHEM_BIN_TABLE_PARMS",	LBL_NULL };
*/
static LblApiProcess_typ    Label = {
    LabelTbl,   "PROPERTY", "PROPERTY",
    LBL_PDS_STRING_NULL,    LBL_NULL };

/******************************************************************************
 *				_LBL_CHEM_BIN_TABLE
 *
 *****************************************************************************/
/*int     LblChemBinTable(
  int   Unit,
  int   Obtain,
  LblChemBinTable_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblChemBinTable_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}*/
int     LblChemBinTable(
  int   Unit,
  int   Obtain,
  LblChemBinTable_typ      *LabelItems,
  int   Instance,
  const char* propertyName)
{ int   RtnStatus;

 if (propertyName!=NULL)
   Label.NameValue = propertyName;

  LblApiCntrl_typ   Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblChemBinTable_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}
/******************************************************************************
 *				LBL_PRINT_CHEM_BIN_TABLE
 *
 *****************************************************************************/
void	LblPrintChemBinTable(
  LblChemBinTable_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}
/******************************************************************************
 *				LBL_TEST_CHEM_BIN_TABLE
 *
 *****************************************************************************/
void	LblTestChemBinTable(
  LblChemBinTable_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
