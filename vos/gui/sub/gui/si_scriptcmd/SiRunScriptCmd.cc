////////////////////////////////////////////////////////////
// SiRunScriptCmd.cc:  Runs a script (usually determined by resources)
// that is passed information about the state of the image display.
//
// Important Note:  The passed-in ImageData *MUST* be an instance of
// RotatedImageData, in order to support the rotation modes (unlike Java,
// we can't do instanceof, sigh).  This is true in xvd, but be careful
// when using this command in other places.  It would be easy enough to
// comment out the rotate stuff (or include a boolean parameter to turn
// on and off) if this ever becomes an issue.
//
// The information is passed to the script in the form of a temporary file
// (which is the only argument to the script).  Inside the file are
// keyword=value pairs.  Each pair is on a line by itself and contains no
// internal spaces.  The list of keywords may be expanded in the future.
// The current set is defined below.  Unless otherwise specified, the value
// is exactly as defined in the Image Widget docs, and the values are
// integers.
//
// scriptVersion=1.0		gives some hint as to what's in the script
// imageMode=bw			value is color or bw
// viewWidth
// viewHeight
// imageWidth
// imageHeight
// xPan
// yPan
// xSubpixelPan
// ySubpixelPan
// xZoomIn
// xZoomOut
// yZoomIn
// yZoomOut
// rawDataMin			double
// rawDataMax			double
// cursorX			double (really cursorXfp resource)
// cursorY			double (really cursorYfp resource)
// displayBounds=(x1,y1,x2,y2)	results of XvicImageDisplayBounds() call
// filename="string"		see below
// rotateMode			value is none, cw, ccw, 180, flip_nw_se,
//				or flip_ne_sw (see RotatedImageData)
// unrotatedDisplayBounds=(x1,y1,x2,y2)
//				displayBounds in original (unrotated) image
// endOfFile=1			always the last value
//
// The filename is exactly as sent in to the ImageData class internally.
// For VICAR files, this means that it could be a comma-separated list of
// three files, in the order red, green, blue.  If the second or third
// filename starts with a ".", then it is an extension only, and the basename
// of the first input should be used.  In addition, any of the three
// components (or the one component in the case of bw) could end in (n),
// indicating that band n should be used from that file.
//
// Note that the cursor location (and pan value) is returned in 0-based
// coordinates, as the Image Widget specifies.  Most programs, including
// xvd, use 1-based coordinates (the xvd Line/Samp report is 1-based).
// To get the same value, take the floating-point cursor location, round
// to the nearest integer, and add 1.
//
// The interactive program hangs while the script is executing, so don't
// do anything that takes too long.  Also, the temporary file is deleted
// as soon as the script returns, so if you are doing background processing,
// you must copy the file first.
///////////////////////////////////////////////////////////

#include "SiRunScriptCmd.h"
#include "ImageData.h"
#include "RotatedImageData.h"
#include "XvicImage.h"
#include "UIComponent.h"		// only for strdup()
#include "Application.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

SiRunScriptCmd::SiRunScriptCmd(const char *name, int active, Widget xiw,
			ImageData *image, const char *script)
		: NoUndoCmd(name, active)
{
   _xiw = xiw;
   _imageData = image;

   if (script)
      _scriptToRun = strdup(script);
   else
      _scriptToRun = NULL;

}

void SiRunScriptCmd::doit()
{
   char temp_file[256];
   FILE *tfp;			// yeah yeah, should probably use streams...
   char cmd[256];

   if (_scriptToRun == NULL || strlen(_scriptToRun) == 0) {
      fprintf(stderr, "No script available to run!\n");
      return;
   }

   theApplication->setBusyCursor();

   // Create temporary file for script params

#ifdef __VMS
   tmpnam(temp_file);
#else
   char *s = tempnam(NULL,NULL);
   if (s) {
      strcpy(temp_file, s);
      free(s);
   }
   else {
      fprintf(stderr, "Unable to allocate temporary filename!\n");
      theApplication->removeBusyCursor();
      return;
   }
#endif

   tfp = fopen(temp_file, "w");
   if (tfp == NULL) {
      fprintf(stderr, "Unable to create temp file '%s'\n", temp_file);
      theApplication->removeBusyCursor();
      return;
   }

   printVersionString(tfp);

   printContents(tfp);

   // Put in end marker

   fprintf(tfp, "endOfFile=1\n");

   // Close file, and build up command string to submit

   fclose(tfp);

   sprintf(cmd, "%s %s", _scriptToRun, temp_file);

   // Now run the command.  We really don't care about the return status.

   (void)system(cmd);

   //!!!! Should capture the output for display...

   // Let subclasses clean up...

   cleanup();

   // Delete the temp file, and we're done

#ifdef __VMS
   (void)remove(temp_file);
#else	/* SunOS doesn't have remove(), and VMS doesn't have unlink().  sigh */
   (void)unlink(temp_file);
#endif

   theApplication->removeBusyCursor();
}

////////////////////////////////////////////////////////////
// Print the version string to the temp file.  This function
// should be overridden by subclasses.
////////////////////////////////////////////////////////////

void SiRunScriptCmd::printVersionString(FILE *tfp)
{
   fprintf(tfp, "scriptVersion=1.0\n");
}

////////////////////////////////////////////////////////////
// Print the contents to the temp file.  This function could
// be overridden by subclasses, which should call this specific
// version to output the basic info.
////////////////////////////////////////////////////////////

void SiRunScriptCmd::printContents(FILE *tfp)
{

   // Get values from image widget

   unsigned char imageMode;
   Dimension viewWidth, viewHeight;
   int imageWidth, imageHeight;
   int xPan, yPan, xSubpixelPan, ySubpixelPan;
   int xZoomIn, xZoomOut, yZoomIn, yZoomOut;
   double rawDataMin, rawDataMax;
   double cursorXfp, cursorYfp;

   XtVaGetValues(_xiw,
	XvicNimageMode, &imageMode,
	XvicNviewWidth, &viewWidth, XvicNviewHeight, &viewHeight,
	XvicNimageWidth, &imageWidth, XvicNimageHeight, &imageHeight,
	XvicNxPan, &xPan, XvicNyPan, &yPan,
	XvicNxSubpixelPan, &xSubpixelPan, XvicNySubpixelPan, &ySubpixelPan,
	XvicNxZoomIn, &xZoomIn, XvicNxZoomOut, &xZoomOut,
	XvicNyZoomIn, &yZoomIn, XvicNyZoomOut, &yZoomOut,
	XvicNrawDataMin, &rawDataMin, XvicNrawDataMax, &rawDataMax,
	XvicNcursorXfp, &cursorXfp, XvicNcursorYfp, &cursorYfp,
	NULL);

   // Write them to the file

   fprintf(tfp, "imageMode=%s\n", imageMode==XvicCOLOR ? "color" : "bw");
   fprintf(tfp, "viewWidth=%d\n", viewWidth);
   fprintf(tfp, "viewHeight=%d\n", viewHeight);
   fprintf(tfp, "imageWidth=%d\n", imageWidth);
   fprintf(tfp, "imageHeight=%d\n", imageHeight);
   fprintf(tfp, "xPan=%d\n", xPan);
   fprintf(tfp, "yPan=%d\n", yPan);
   fprintf(tfp, "xSubpixelPan=%d\n", xSubpixelPan);
   fprintf(tfp, "ySubpixelPan=%d\n", ySubpixelPan);
   fprintf(tfp, "xZoomIn=%d\n", xZoomIn);
   fprintf(tfp, "xZoomOut=%d\n", xZoomOut);
   fprintf(tfp, "yZoomIn=%d\n", yZoomIn);
   fprintf(tfp, "yZoomOut=%d\n", yZoomOut);
   fprintf(tfp, "rawDataMin=%f\n", rawDataMin);
   fprintf(tfp, "rawDataMax=%f\n", rawDataMax);
   fprintf(tfp, "cursorX=%f\n", cursorXfp);
   fprintf(tfp, "cursorY=%f\n", cursorYfp);

   // Put in view bounds

   int x1, y1, x2, y2;
   XvicImageDisplayBounds(_xiw, &x1, &y1, &x2, &y2);
   fprintf(tfp, "displayBounds=(%d,%d,%d,%d)\n", x1, y1, x2, y2);

   // Put in filenames.  Note:  the filenames are given as xvd sees them.
   // For VICAR files this means comma separated names.  If the second
   // or third name starts with ".", use the basename of the first.
   // If a (n) exists after the name, that indicates which band to use.

   char *filename;
   filename = (char *)_imageData->getInputDataSourceName();
   fprintf(tfp, "filename=\"%s\"\n", filename ? filename : "");

   // Put in rotation mode.  This section could be commented out, or controlled
   // by a boolean, if the image were not a RotatedImageData (in which case
   // rotationMode=none should be output, and unrotatedDisplayBounds should
   // be output the same as displayBounds).

   RotatedImageData *rot = (RotatedImageData *)_imageData;
   switch (rot->getRotationMode()) {
      case ROTATE_CW:
         fprintf(tfp, "rotateMode=cw\n");
         break;
      case ROTATE_CCW:
         fprintf(tfp, "rotateMode=ccw\n");
         break;
      case ROTATE_FULL:
         fprintf(tfp, "rotateMode=180\n");
         break;
      case FLIP_NW_SE:
         fprintf(tfp, "rotateMode=flip_nw_se\n");
         break;
      case FLIP_NE_SW:
         fprintf(tfp, "rotateMode=flip_ne_sw\n");
         break;
      case ROTATE_NO:
      default:
         fprintf(tfp, "rotateMode=none\n");
         break;
   }

   int w = x2 - x1 + 1;
   int h = y2 - y1 + 1;
   rot->getUnrotTileCoords(&x1, &y1, &w, &h);
   x2 = x1 + w - 1;
   y2 = y1 + h - 1;
   fprintf(tfp, "unrotatedDisplayBounds=(%d,%d,%d,%d)\n", x1, y1, x2, y2);

}

