////////////////////////////////////////////////////////////
// SiPrintCmd.h:  Prints the image according to the SiPrintCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiSaveAsCmd, adding the SiPrintCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! print.  It should be done internally, via save hooks in ImageData.
//
// In adition to the information included by SiSaveAsCmd, the
// following information is passed:
//
// scriptVersion=1.3		replaces 1.2 from base class
// printTo=printer		where to print: printer or file
// printCommand="string"	command to use to print
// printWidth=6.0		width of printed image, in inches.  Blank is
//				okay (width is automatically determined)
// printHeight=6.0		height of image, in inches.  Blank is okay
// orientation=tall		orientation of page: tall (portrait) or
//				wide (landscape)
// printTitle="string"		title string to print, or blank for no title
///////////////////////////////////////////////////////////

#include "SiPrintCmd.h"
#include "SiPrintCmdValue.h"
#include "ImageData.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////

SiPrintCmd::SiPrintCmd(const char *name, int active, Widget xiw,
		ImageData *image,char *script,
                Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB)
        : SiSaveAsCmd(name, active, xiw, image, script,
                sR, sG, sB, pR, pG, pB)
{
   // Empty
}

////////////////////////////////////////////////////////////
// Print the version string to the temp file.  This function
// should be overridden by subclasses.
////////////////////////////////////////////////////////////

void SiPrintCmd::printVersionString(FILE *tfp)
{
   fprintf(tfp, "scriptVersion=1.3\n");
}

////////////////////////////////////////////////////////////
// Print the contents to the temp file.  This function could
// be overridden by subclasses, which should call this specific
// version to output the basic info.
////////////////////////////////////////////////////////////

void SiPrintCmd::printContents(FILE *tfp)
{
   // Print basic values

   SiSaveAsCmd::printContents(tfp);

   // Get extra values from CmdValue

   SiPrintCmdValue *value = (SiPrintCmdValue *)_value;

   if (value == NULL) {
      fprintf(stderr,"No SiPrintCmdValue, internal error, file not printed!\n");
      return;
   }

   switch (value->print_to) {
      case PrintToPrinter:
         fprintf(tfp, "printTo=printer\n");
         break;
      case PrintToFile:
         fprintf(tfp, "printTo=file\n");
         break;
      default:
         break;
   }
   fprintf(tfp, "printCommand=\"%s\"\n", value->print_command);
   fprintf(tfp, "printWidth=%s\n", value->print_width);
   fprintf(tfp, "printHeight=%s\n", value->print_height);

   switch (value->orientation) {
      case PrintPortrait:
         fprintf(tfp, "orientation=tall\n");
         break;
      case PrintLandscape:
         fprintf(tfp, "orientation=wide\n");
         break;
      default:
         break;
   }
   if (value->title_filename) {		// use filename for title
      char *filename, fn[512];
      filename = (char *)_imageData->getInputDataSourceName();
      if (filename == NULL)	 // BW files end in ",,".  Cut this off.
         strcpy(fn, "");
      else
         strcpy(fn, filename);
      if (fn[strlen(fn)-1] == ',' && fn[strlen(fn)-2] == ',')
         fn[strlen(fn)-2] = '\0';
      fprintf(tfp, "printTitle=\"%s\"\n", fn);
   }
   else {				// custom title
      fprintf(tfp, "printTitle=\"%s\"\n", value->print_title);
   }

}

////////////////////////////////////////////////////////////
// Delete the CmdValue object
////////////////////////////////////////////////////////////

void SiPrintCmd::freeValue(CmdValue value)
{
   if (value)
      delete (SiPrintCmdValue *)value;
}

