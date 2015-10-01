$!****************************************************************************
$!
$! Build proc for MIPL module histglue_h
$! VPACK Version 1.8, Friday, July 26, 1996, 14:06:31
$!
$! Execute by entering:		$ @histglue_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module histglue_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to histglue_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("histglue_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @histglue_h.bld "STD"
$   else
$      @histglue_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histglue_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histglue_h.com -mixed -
	-s ImageToHistGlue.h RawHistToStrHistGlue.h LutToStrHistGlue.h -
	   CollectStretchedHist.h CollectHist.h CollectHistBG.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ImageToHistGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToHistGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Histogram objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it recollects the histograms (which in turn cause them to update their
// own views).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETOHISTGLUE_H
#define IMAGETOHISTGLUE_H
#include "BasicImageView.h"

class ImageData;
class Histogram;

class ImageToHistGlue : public BasicImageView {

 private:

 protected:
   Histogram *_histR;
   Histogram *_histG;
   Histogram *_histB;

   void *_collectionActive;

 public:

   ImageToHistGlue (ImageData *model,
			Histogram *histR, Histogram *histG, Histogram *histB);
   virtual ~ImageToHistGlue ();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToHistGlue"; }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create RawHistToStrHistGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// RawHistToStrHistGlue: class that serves as a "glue" class between a
// set of Histogram objects (the unstretched ones) and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the unstretched Histogram, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef RAWHISTTOSTRHISTGLUE_H
#define RAWHISTTOSTRHISTGLUE_H
#include "HistView.h"

class Histogram;
class Lut;

class RawHistToStrHistGlue : public HistView {

 protected:

   Histogram *_strhistR;
   Histogram *_strhistG;
   Histogram *_strhistB;

   Lut *_lutR;
   Lut *_lutG;
   Lut *_lutB;

 public:

   RawHistToStrHistGlue (
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "RawHistToStrHistGlue"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutToStrHistGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// LutToStrHistGlue: class that serves as a "glue" class between a
// set of LUT objects and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the LUT, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef LUTTOSTRHISTGLUE_H
#define LUTTOSTRHISTGLUE_H
#include "LutView.h"

class Histogram;

class LutToStrHistGlue : public LutView {

 protected:

   Histogram *_histR;
   Histogram *_histG;
   Histogram *_histB;
   Histogram *_strhistR;
   Histogram *_strhistG;
   Histogram *_strhistB;

 public:

   LutToStrHistGlue ( 
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "LutToStrHistGlue"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CollectStretchedHist.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
//  CollectStretchedHist.h
//////////////////////////////////////////////////////////////////////
#ifndef COLLECTSTRETCHEDHIST_H
#define COLLECTSTRETCHEDHIST_H

class Lut;
class Histogram;

void CollectStretchedHist ( 
	Histogram *histR, Histogram *histG, Histogram *histB,
	Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
	Lut *lutR, Lut *lutG, Lut* lutB);


void CollectStretchedHist ( Histogram *hist, Histogram *strhist, Lut *lut);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CollectHist.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CollectHist.h: These subroutines can be used to fill in histogram 
// model.  The caller should pass a pointer to existing histogram 
// model object.
////////////////////////////////////////////////////////////////
#ifndef COLLECTHIST_H
#define COLLECTHIST_H

#include "ImageDefs.h"

class ImageData;
class Histogram;

#define HISTSIZE 256

// Calculate histogram from the image

void CollectHist(ImageData *imageModel, 
		Histogram *histR, Histogram *histG, Histogram *histB);
void getHistPtr (Histogram *, ImageData *, int, int, int, ColorType);


// Calculate histogram from the array of size HISTSIZE

void CollectHist(int arrayR[], int arrayG[], int arrayB[],
                Histogram *histR, Histogram *histG, Histogram *histB);


// Calculate histogram from the array of arbitrary size

void CollectHist(int *arrayR, int *arrayG, int *arrayB, int size,
                Histogram *histR, Histogram *histG, Histogram *histB);

void getHistPtr (Histogram *, int *array, int size);

// Actually read the pixels for one line's worth and update the histogram

void CollectHistLine(Histogram *hist, unsigned char *buffer, int size,
		ImagePixelType type);

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CollectHistBG.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CollectHistBG.h
////////////////////////////////////////////////////////////////
#ifndef COLLECTHISTBG_H
#define COLLECTHISTBG_H

class ImageData;
class Histogram;
class ImageTile;

void CollectHistBG(ImageData *imageModel, 
	Histogram *histR, Histogram *histG, Histogram *histB, void **active);

void CollectHistFromTile(Histogram *hist, int bufferIndex, ImageTile &tile,
	int width, int height);

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
