$!****************************************************************************
$!
$! Build proc for MIPL module pds_image_h
$! VPACK Version 1.9, Thursday, August 14, 2003, 19:29:44
$!
$! Execute by entering:		$ @pds_image_h
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
$ write sys$output "*** module pds_image_h ***"
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
$ write sys$output "Invalid argument given to pds_image_h.com file -- ", primary
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
$   if F$SEARCH("pds_image_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @pds_image_h.bld "STD"
$   else
$      @pds_image_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pds_image_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pds_image_h.com -mixed -
	-s PdsImageData.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PdsImageData.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// PdsImageData.h
//
//	This is a subclass of ImageData.  It retrieves data
//	from Pds formatted files.  
//
////////////////////////////////////////////////////////////////
#ifndef PDSIMAGEDATA_H
#define PDSIMAGEDATA_H

#include "ImageData.h"

extern "C" {
 #include "oal.h"
}

#include <Xm/Xm.h>

class ZoomFactor;

class PdsImageData : public ImageData {

 protected:

   // INFO ABOUT EACH FILE  --- Pds Specific

   int		_numbFiles;   // # of different physical files	
   int		_numbChannels;// #of logical channels, 3 for RGB or 1 for BW
   ODLTREE      _odlTree;

   OA_OBJECT _files[MAX_FILES];	            // hold the file handles (one/band)
   OA_OBJECT _fileForChannel[MAX_CHANS];    // ptr copies for each chan
   int	     _bandForChannel[MAX_CHANS];    // band for each channel

   // INIT VALUES IN FILES

   StatusType initNumbers();	   // inits bands,lines,samples, etc

   // FILE MANAGEMENT

   virtual StatusType addFile( char *, int);
   //Opens one band of PDS Image File
   virtual StatusType validateFile(OA_OBJECT);
   virtual void readDataRange();		    

 public:

   PdsImageData();
   virtual ~PdsImageData();

   // FILE ROUTINES:

   virtual StatusType open(char *);
   virtual StatusType close();	

   virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);

   // READ DATA

   virtual StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
			       int unzoomedWidth, int unzoomedHeight,
			       ImageTile &tile);

   virtual StatusType readLine(ColorType color, int line,
			       unsigned char * bufferPtr);

   virtual StatusType readPixel(ColorType color, int sampleOffset,
				int lineOffset, unsigned char * pixelBuffer);
   
   virtual StatusType read1Tile1Band(int, int, int, int, int, ZoomFactor &,
				     unsigned char *, int);
   // Label display
   ImageLabel *buildLabelTree(ImageData *image, ODLTREE odltree, char *parentkey);
   virtual StatusType getLabelSetValue(char*& labels, char *key, int *maxsize);
   ODLTREE parseLabelKey(char *key);
   StatusType PdsPrintLabel(char*& labels, ODLTREE labelNode, int *maxsize);

   virtual const char *className() { return ("PdsImageData"); }
};

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
