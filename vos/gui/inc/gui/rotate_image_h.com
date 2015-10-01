$!****************************************************************************
$!
$! Build proc for MIPL module rotate_image_h
$! VPACK Version 1.9, Thursday, March 01, 2001, 10:24:56
$!
$! Execute by entering:		$ @rotate_image_h
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
$ write sys$output "*** module rotate_image_h ***"
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
$ write sys$output "Invalid argument given to rotate_image_h.com file -- ", primary
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
$   if F$SEARCH("rotate_image_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @rotate_image_h.bld "STD"
$   else
$      @rotate_image_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rotate_image_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rotate_image_h.com -mixed -
	-s RotatedImageData.h RotationDefs.h SiRotateImageCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create RotatedImageData.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
//
//      RotatedImageData.h
//
//	This is a class derived from ImageDataWrapper class. 
//	It will rotate the wrapped image in a number of ways
//	(see RotationDefs.h).
//
////////////////////////////////////////////////////////////////
#ifndef ROTATEDIMAGEDATA_H
#define ROTATEDIMAGEDATA_H
#include "ImageDataWrapper.h"
#include "RotationDefs.h" 

class RotatedImageData : public ImageDataWrapper { 

  protected:

    RotationType _rotationMode;    // left, right, flip

    void copyTileBuffer(ImageTile *rotTile, ImageTile *unrotTile,
			int height, int width);

    int getUnrotTile_x (int x);
    int getUnrotTile_y (int y);
    int getUnrotTile_x (double x);
    int getUnrotTile_y (double y);

  public:

    RotatedImageData();
    RotatedImageData(RotationType mode);
    virtual ~RotatedImageData();

    // Functions specific to this subclass

    RotationType getRotationMode() { return(_rotationMode); };
    void setRotationMode(RotationType rotMode);

    // Overrides of superclass functions

    // Surprisingly, open/close require no overrides.  All rotations are
    // handled in the various other routines.

    void transDisplayToImageCoords(int x, int y, int *Sample, int *Line);
    void transDisplayToImageCoords(double x, double y, 
				   double *sample, double *line);
    void transImageToDisplayCoords(int sample, int line, int *x, int *y);
    void transImageToDisplayCoords(double sample, double line,
				   double *x, double *y);

    virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);
    virtual void getUnzoomedTileSize(int &height, int &width);
    virtual void setUnzoomedTileSize(int height, int width);

    virtual ZoomFactor &calcTileZoom(ImageTile *tile,
				     int Xin=1, int Xout=1, 
				     int Yin=1, int Yout=1, 
				     int Xpan = 0, int Ypan = 0);    
    virtual ImageTile &createTile(ZoomFactor &tileZoom,
				  Boolean willCreateBuffers = TRUE);
    // getTileZoom() does not need to be overridden.  The ImageTile object is
    // always the rotated one; unrotated tiles are not seen by the caller.
    // All getTileZoom() does is to call TileZoom::getTileZoom() and really
    // should not exist.

    virtual int getNumbSamples() const;
    virtual int getNumbLines() const;

    virtual int lineSampToLatLon(double line, double samp, double *lat,
				 double *lon, int type);
    virtual int latLonToLineSamp(double *line, double *samp, double lat,
				 double lon, int type);

    // Read image data

    StatusType readPixel(ColorType color, int x, int y, 
			 unsigned char *bufferPtr);
    StatusType readLine(ColorType color, int line, 
			unsigned char *bufferPtr);
    StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
			int unzoomedWidth, int unzoomedHeight,
			ImageTile &tile);

    // Given the coordinates of a tile, return the corresponding coordinates
    // of the same tile in the unrotated (original) image

    void getUnrotTileCoords(int *x, int *y, int *w, int *h);
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create RotationDefs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// RotationDefs.h 
//
////////////////////////////////////////////////////////////////
#ifndef ROTATIONDEFS_H
#define ROTATIONDEFS_H

enum    RotationType    { ROTATE_NO, ROTATE_CW, ROTATE_CCW,
                          FLIP_NW_SE, FLIP_NE_SW, ROTATE_FULL };

#define ROT_NO_SWAP(type) (((type)==ROTATE_NO) || ((type)==ROTATE_FULL))

// Define X resource names

#define XvicNrotation             "rotation"
#define XvicCRotation             "Rotation"
#define XvicRRotationType         "RotationType"

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiRotateImageCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// SiRotateImageCmd.h: Rotate image.
////////////////////////////////////////////////////////////
#ifndef SiRotateImageCmd_H
#define SiRotateImageCmd_H
#include "RadioCmd.h"
#include "RotationDefs.h"

class RotatedImageData;

class SiRotateImageCmd : public RadioCmd {

  protected:

    RotationType _rotation;
    RotatedImageData *_image;

    virtual void doit();
    
  public:

    SiRotateImageCmd(const char *, int, CmdList *,
					RotatedImageData *, RotationType);

    virtual const char *const className () { return "SiRotateImageCmd"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
