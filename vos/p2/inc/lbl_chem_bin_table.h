#ifndef MIPS_LBL_CHEM_BIN_TABLE_INCLUDED
#define MIPS_LBL_CHEM_BIN_TABLE_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

  /**  Copyright (c) 2002, California Institute of Technology             **/
  /**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

  /******************************************************************************
   *				LBL_CHEM_BIN_TABLE
   *
   *	This module contains routines to help create, read/write and print
   *  a ChemBinTable property label.  It is part of the MIPL label API
   *  package, using a lower-level label processor to do the real work.  This
   *  package basically defines a table that the lower-level routines use.
   *  The table is the bridge between how the application access the label
   *  elements, and how the label processor specifies the label components
   *  to the VICAR label Run Time Library (RTL).
   *
   *	The primary routine used by a typical application program is
   *  LblChemBinTable.  This routine requires exactly 4 parameters.
   *  All label API routines  have the same four parameters:
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
   *============================================================================
   * History of modifications:
   * ------------------------ 
   * Date         Who             Description
   * -----------  --------------- ---------------------------------------------
   *****************************************************************************/
#define IDPH_TABLE_PARMS_PROPERTY_NAME          "IDPH_TABLE"
#define ANCILLARY_TABLE_PARMS_PROPERTY_NAME     "ANCILLARY_TABLE"
#define CMD_REPLY_FRAME_TABLE_PARMS_PROPERTY_NAME     "CMD_REPLY_FRAME_TABLE"
#define TAKE_IMAGE_TIME_TABLE_PARMS_PROPERTY_NAME     "TAKE_IMAGE_TIME_TABLE"
#define SOH_SCIDATA_COLS_TABLE_PARMS_PROPERTY_NAME    "SOH_SCIDATA_COLS_TABLE"
#define SOH_TO_RCE_TABLE_PARMS_PROPERTY_NAME    "SOH_TO_RCE_TABLE"
#define SOH_CHECKSUM_TABLE_PARMS_PROPERTY_NAME    "SOH_CHECKSUM_TABLE"
#define SOH_BEFORE_TABLE_PARMS_PROPERTY_NAME     "SOH_BEFORE_TABLE"
#define SOH_AFTER_TABLE_PARMS_PROPERTY_NAME     "SOH_AFTER_TABLE"
#define AUTOFOCUS_TABLE_PARMS_PROPERTY_NAME     "AUTOFOCUS_TABLE"
#define MUHEADER_TABLE_PARMS_PROPERTY_NAME     "MUHEADER_TABLE"
#define MUFOOTER_TABLE_PARMS_PROPERTY_NAME     "MUFOOTER_TABLE"
#define IMAGE_REPLY_TABLE_PARMS_PROPERTY_NAME   "IMAGE_REPLY_TABLE"
#define IMAGE_HF_TABLE_PARMS_PROPERTY_NAME   "IMAGE_HEADER_FOOTER_TABLE"

  typedef struct
  {
    LblApiStringItem_typ		PdsObjectType;
    LblApiIntItem_typ           PdsObjectPtr;
    LblApiIntItem_typ           PdsObjectOffset;
    LblApiStringItem_typ		PdsObjectLoc;
    LblApiIntItem_typ           Bytes;
    LblApiStringItem_typ        InterchangeFormat;
    LblApiIntItem_typ           Rows;
    LblApiIntItem_typ           Columns;
    LblApiIntItem_typ           RowBytes;
    LblApiStringItem_typ        Description;
    LblApiStringItem_typ        StructurePtr;
  } LblChemBinTable_typ;

  int	LblChemBinTable( int, int, LblChemBinTable_typ *, int,const char*  );
  void	LblTestChemBinTable( LblChemBinTable_typ *);
  void	LblPrintChemBinTable( LblChemBinTable_typ *);

#ifdef __cplusplus
}
#endif

#endif
