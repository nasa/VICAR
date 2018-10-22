////////////////////////////////////////////////////////////////
// SiPrintCmdValue.h:  Contains the parameters needed for the Print
// command.  This object is passed to the Print command as a value.
////////////////////////////////////////////////////////////////
#ifndef SIPRINTCMDVALUE_H
#define SIPRINTCMDVALUE_H

#include "SiSaveCmdValue.h"

enum SiPrintDestination { PrintToPrinter, PrintToFile };
enum SiPrintOrientation { PrintPortrait, PrintLandscape };

struct SiPrintCmdValue : public SiSaveCmdValue {

    SiPrintDestination print_to;
    char print_command[256];
    char print_width[20];
    char print_height[20];
    SiPrintOrientation orientation;
    Boolean title_filename;
    char print_title[256];

    SiPrintCmdValue() : SiSaveCmdValue()		// Set defaults
	{  print_to = PrintToPrinter;  strcpy(print_command, "lp -c");
	   strcpy(print_width, "");  strcpy(print_height, "");
	   orientation = PrintPortrait;  strcpy(print_title, "");
	   title_filename = True;
	}

};

#endif

