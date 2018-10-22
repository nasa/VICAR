////////////////////////////////////////////////////////////
// SiRunStretchScriptCmd.h:  Runs a script (usually determined
// by resources) that is passed information about the state of
// the image display.  This subclass adds information about the
// stretch and pseudocolor tables to the basic SiRunScriptCmd.
//
// In adition to the information included by SiRunScriptCmd, the
// following information is passed:
//
// scriptVersion=1.1		replaces 1.0 from base class
// lutType=raw			values are raw, stretch, pseudo, pseudo_only
// dataType=byte		values are byte, half, uhalf, full, ufull,
//					   real, double
// stretchFile="string"		temporary file containing stretch tables
// pseudoFile="string"		temporary file containing pseudo tables
//
// Note that pseudoFile is only written when imageMode==bw and lutType
// == pseudo or pseudo_only.  Both stretch and pseudo files are in IBIS
// format, as written by xvd.  The stretch and pseudo files are deleted
// as soon as the script returns, so if you are doing background processing,
// you must copy them first.
///////////////////////////////////////////////////////////

#include "SiRunStretchScriptCmd.h"
#include "XvicImage.h"
#include "SaveLutFileCmd.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

SiRunStretchScriptCmd::SiRunStretchScriptCmd(const char *name, int active,
			Widget xiw, ImageData *image, const char *script,
			Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB)
		: SiRunScriptCmd(name, active, xiw, image, script)
{
   _lutRed = sR;
   _lutGrn = sG;
   _lutBlu = sB;
   _pseudoRed = pR;
   _pseudoGrn = pG;
   _pseudoBlu = pB;
}

////////////////////////////////////////////////////////////
// Print the version string to the temp file.  This function
// should be overridden by subclasses.
////////////////////////////////////////////////////////////

void SiRunStretchScriptCmd::printVersionString(FILE *tfp)
{
   fprintf(tfp, "scriptVersion=1.1\n");
}

////////////////////////////////////////////////////////////
// Print the contents to the temp file.  This function could
// be overridden by subclasses, which should call this specific
// version to output the basic info.
////////////////////////////////////////////////////////////

void SiRunStretchScriptCmd::printContents(FILE *tfp)
{
   // Print basic values

   SiRunScriptCmd::printContents(tfp);

   // Get extra values from image widget

   unsigned char lutType, dataType;

   XtVaGetValues(_xiw,
	XvicNlutType, &lutType, XvicNdataType, &dataType, NULL);

   // Write them to the file

   switch (lutType) {
      case XvicRAW:
         fprintf(tfp, "lutType=raw\n");
         break;
      case XvicSTRETCH:
         fprintf(tfp, "lutType=stretch\n");
         break;
      case XvicPSEUDO:
         fprintf(tfp, "lutType=pseudo\n");
         break;
      case XvicPSEUDO_ONLY:
         fprintf(tfp, "lutType=pseudo_only\n");
         break;
      default:
         break;
   }

   switch (dataType) {
      case XvicBYTE:
         fprintf(tfp, "dataType=byte\n");
         break;
      case XvicHALF:
         fprintf(tfp, "dataType=half\n");
         break;
      case XvicUHALF:
         fprintf(tfp, "dataType=uhalf\n");
         break;
      case XvicFULL:
         fprintf(tfp, "dataType=full\n");
         break;
      case XvicUFULL:
         fprintf(tfp, "dataType=ufull\n");
         break;
      case XvicREAL:
         fprintf(tfp, "dataType=real\n");
         break;
      case XvicDOUBLE:
         fprintf(tfp, "dataType=double\n");
         break;
      default:
         break;
   }

   // Create temporary file for stretch

#ifdef __VMS
   tmpnam(_stretch_file);
#else
   char *s = tempnam(NULL, NULL);
   if (s) {
      strcpy(_stretch_file, s);
      free(s);
   }
   else {
      fprintf(stderr, "Unable to allocate temporary filename!\n");
      return;
   }
#endif

   // Create file save command

   SaveLutFileCmd *cmd = new SaveLutFileCmd("Save Stretch", TRUE,
			_lutRed, _lutGrn, _lutBlu);

   // Run it, print the name to the file, and nuke the command

   if (cmd) {
      cmd->execute((CmdValue)_stretch_file);
      delete cmd;

      fprintf(tfp, "stretchFile=\"%s\"\n", _stretch_file);
   }

   // Now save the Pseudo tables, in the same way

   if (lutType == XvicPSEUDO || lutType == XvicPSEUDO_ONLY) {

#ifdef __VMS
      tmpnam(_pseudo_file);
#else
      char *p = tempnam(NULL, NULL);
      if (p) {
         strcpy(_pseudo_file, p);
         free(p);
      }
      else {
         fprintf(stderr, "Unable to allocate temporary filename!\n");
         return;
      }
#endif

      cmd = new SaveLutFileCmd("Save Pseudo", TRUE,
			_pseudoRed, _pseudoGrn, _pseudoBlu);

      if (cmd) {
         cmd->execute((CmdValue)_pseudo_file);
         delete cmd;

         fprintf(tfp, "pseudoFile=\"%s\"\n", _pseudo_file);
      }
   }
   else
      strcpy(_pseudo_file, "");			// for cleanup()
}

////////////////////////////////////////////////////////////
// Delete the temporary LUT files, after the script is done...
////////////////////////////////////////////////////////////

void SiRunStretchScriptCmd::cleanup()
{
#ifdef __VMS
   (void)remove(_stretch_file);
   if (strlen(_pseudo_file) > 0)
      (void)remove(_pseudo_file);
#else	/* SunOS doesn't have remove() and VMS doesn't have unlink().  sigh */
   (void)unlink(_stretch_file);
   if (strlen(_pseudo_file) > 0)
      (void)unlink(_pseudo_file);
#endif
}

