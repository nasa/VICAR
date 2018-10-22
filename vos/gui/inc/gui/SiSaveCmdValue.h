////////////////////////////////////////////////////////////////
// SiSaveCmdValue.h:  Contains the parameters needed for the Save
// (really SaveAs) command.  This object is passed to the Save
// command as a value.
////////////////////////////////////////////////////////////////
#ifndef SISAVECMDVALUE_H
#define SISAVECMDVALUE_H

#include "XvicBasicImage.h"

enum SiSaveImageExtent { SaveDisplayOnly, SaveEntireFile, SaveROI };

struct SiSaveCmdValue {

    char filename_red[256];
    char filename_grn[256];
    char filename_blu[256];
    SiSaveImageExtent imageExtent;
    unsigned char lutType;	// See Xiw lutType for values
    Boolean asByte;		// T: convert to byte, stretches allowed
				// F: preserve data type, no str unless byte
    char fileFormat[33];	// e.g. VICAR

    SiSaveCmdValue()		// Set defaults
	{  strcpy(filename_red, ""); strcpy(filename_grn, ".grn");
	   strcpy(filename_blu, ".blu"); imageExtent = SaveDisplayOnly;
	   lutType = XvicSTRETCH; asByte = True;
	   strcpy(fileFormat, "VICAR");		//!!!!
	}

};

#endif

