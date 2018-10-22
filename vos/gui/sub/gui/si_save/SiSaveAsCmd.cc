////////////////////////////////////////////////////////////
// SiSaveAsCmd.h:  Saves the image according to the SiSaveCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiRunStretchScriptCmd, adding the SiSaveCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! save.  It should be done internally, via save hooks in ImageData.
//
// In adition to the information included by SiRunStretchScriptCmd, the
// following information is passed:
//
// scriptVersion=1.2		replaces 1.1 from base class
// saveFilename="string"	see below
// saveImageExtent=file		how much to save: display, file, roi
// saveLutType=stretch		whether to use stretch/pseudo tables
//				(same values as lutType in parent)
// saveAsByte=1			0=retain data type (no stretch unless byte)
//				1=convert to byte (stretch/pseudo allowed)
// saveFileFormat="VICAR"	file format to save in, currently VICAR or TIFF
//
// saveFilename follows the same rules as filename in SiRunScriptCmd,
// except that band numbers (in parens) are not allowed.
///////////////////////////////////////////////////////////

#include "SiSaveAsCmd.h"
#include "SiSaveCmdValue.h"
#include "XvicImage.h"
#include "ImageToReloadGlue.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////

SiSaveAsCmd::SiSaveAsCmd(const char *name, int active, Widget xiw,
		ImageData *image, const char *script,
                Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB)
        : SiRunStretchScriptCmd(name, active, xiw, image, script,
                sR, sG, sB, pR, pG, pB)
{
   // Empty
}

////////////////////////////////////////////////////////////
// Print the version string to the temp file.  This function
// should be overridden by subclasses.
////////////////////////////////////////////////////////////

void SiSaveAsCmd::printVersionString(FILE *tfp)
{
   fprintf(tfp, "scriptVersion=1.2\n");
}

////////////////////////////////////////////////////////////
// Print the contents to the temp file.  This function could
// be overridden by subclasses, which should call this specific
// version to output the basic info.
////////////////////////////////////////////////////////////

void SiSaveAsCmd::printContents(FILE *tfp)
{
   // Print basic values

   SiRunStretchScriptCmd::printContents(tfp);

   // Get extra values from CmdValue

   SiSaveCmdValue *value = (SiSaveCmdValue *)_value;

   if (value == NULL) {
      fprintf(stderr, "No SiSaveCmdValue, internal error, file not saved!\n");
      return;
   }

   if (strlen(value->filename_grn) == 0 && strlen(value->filename_blu) == 0)
      fprintf(tfp, "saveFilename=\"%s\"\n", value->filename_red);
   else
      fprintf(tfp, "saveFilename=(\"%s\",\"%s\",\"%s\")\n",
		value->filename_red, value->filename_grn, value->filename_blu);

   switch (value->imageExtent) {
      case SaveDisplayOnly:
         fprintf(tfp, "saveImageExtent=display\n");
         break;
      case SaveEntireFile:
         fprintf(tfp, "saveImageExtent=file\n");
         break;
      case SaveROI:			// Not Implemented!!!!
         fprintf(tfp, "saveImageExtent=roi\n");
         break;
      default:
         break;
   }

   switch (value->lutType) {
      case XvicRAW:
         fprintf(tfp, "saveLutType=raw\n");
         break;
      case XvicSTRETCH:
         fprintf(tfp, "saveLutType=stretch\n");
         break;
      case XvicPSEUDO:
         fprintf(tfp, "saveLutType=pseudo\n");
         break;
      case XvicPSEUDO_ONLY:
         fprintf(tfp, "saveLutType=pseudo_only\n");
         break;
      default:
         break;
   }

   fprintf(tfp, "saveAsByte=%d\n", value->asByte ? 1 : 0);
   fprintf(tfp, "saveFileFormat=%s\n", value->fileFormat);

}

////////////////////////////////////////////////////////////
// Delete the CmdValue object
////////////////////////////////////////////////////////////

void SiSaveAsCmd::freeValue(CmdValue value)
{
   if (value)
      delete (SiSaveCmdValue *)value;
}

