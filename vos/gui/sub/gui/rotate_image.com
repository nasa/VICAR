$!****************************************************************************
$!
$! Build proc for MIPL module rotate_image
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:32
$!
$! Execute by entering:		$ @rotate_image
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module rotate_image ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to rotate_image.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("rotate_image.imake") .nes. ""
$   then
$      vimake rotate_image
$      purge rotate_image.bld
$   else
$      if F$SEARCH("rotate_image.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rotate_image
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rotate_image.bld "STD"
$   else
$      @rotate_image.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rotate_image.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rotate_image.com -mixed -
	-s RotatedImageData.cc SiRotateImageCmd.cc -
	-i rotate_image.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create RotatedImageData.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
//
//  RotatedImageData.cc
//
//      This is a class derived from ImageDataWrapper class.
//      It will rotate the wrapped image in a number of ways
//	(see RotationDefs.h).
//
////////////////////////////////////////////////////////////////////////
#include "RotatedImageData.h"
#include "ErrorDialogManager.h"
#include <stdio.h>

////////////////////////////////////////////////////////////////////////
//	Constructor
////////////////////////////////////////////////////////////////////////
RotatedImageData::RotatedImageData(RotationType mode) : ImageDataWrapper()
{
    _rotationMode = mode;
}

RotatedImageData::RotatedImageData() : ImageDataWrapper()
{
    _rotationMode = ROTATE_NO;
}

////////////////////////////////////////////////////////////////////////
//	Destructor
////////////////////////////////////////////////////////////////////////
RotatedImageData::~RotatedImageData( )
{
    // Empty
}

////////////////////////////////////////////////////////////////////////
//  computes the unrotated tile coords by using the rotated one 
////////////////////////////////////////////////////////////////////////
void RotatedImageData::getUnrotTileCoords (int *x, int *y, int *w, int *h)
{ 
    int ss, sl, height, width;
    
    ss = *x;
    sl = *y;
    height = *h;
    width = *w;
    
     switch(_rotationMode) { 
      case ROTATE_NO:   *x = ss;
                        *y = sl;
                        *h = height;
                        *w = width;
                        break;

      case ROTATE_CCW:  *x = getNumbLines() - sl - height;
                        *y = ss;
                        *h = width;
                        *w = height;
                        break;

      case ROTATE_CW:   *x = sl;
                        *y = getNumbSamples() - ss - width;
                        *h = width;
                        *w = height;
                        break;

      case FLIP_NW_SE:  *x = getNumbLines() - sl - height;
                        *y = getNumbSamples() - ss - width;
                        *h = width;
                        *w = height;
                        break;

      case FLIP_NE_SW:  *x = sl;
                        *y = ss;
                        *h = width;
                        *w = height;
                        break;
     
      case ROTATE_FULL: *x = getNumbSamples() - ss - width;
                        *y = getNumbLines() - sl - height;
                        *h = height;
                        *w = width;
                        break;
    }
}

////////////////////////////////////////////////////////////////////////
//    copy the content of unrotated tile buffer into the buffer 
//    of the rotated tile 
////////////////////////////////////////////////////////////////////////
void RotatedImageData::copyTileBuffer(ImageTile *rotTile, ImageTile *unrotTile,
                                      int height, int width)
{
    int x, y, max, i;
    unsigned char *unrotBuf = NULL;
    unsigned char *rotBuf;
    int pixSize;
    
    // get the current pixel size from the rotated tile 
    pixSize = rotTile->getPixelSize();
    
    // set "max" related to the image type (BW or color)
    if(unrotTile->getMode() == BWmode)
	max = 0;
    else
	max = 2;

    // copy the contents of the unrotated buffer into the rotated one 
    for(i=0; i<=max; i++) {
	for(y=0; y<height; y++) {
	    for(x=0; x<width; x++) {   
		rotBuf = rotTile->getBufferPtr(i) + y*rotTile->getLineWidth()
		    + x*rotTile->getPixelSize() + rotTile->getByteOffset();

		switch(_rotationMode) { 
		case ROTATE_NO: break;
		    
		case ROTATE_CCW: 
		    unrotBuf = unrotTile->getBufferPtr(i)
			+ x * unrotTile->getLineWidth()
			+ (height-y-1) * unrotTile->getPixelSize() 
			+ unrotTile->getByteOffset();
		    break;
		    
		case ROTATE_CW:        
		    unrotBuf = unrotTile->getBufferPtr(i)
			+ (width-x-1) * unrotTile->getLineWidth()
			+ y * unrotTile->getPixelSize() 
			+ unrotTile->getByteOffset(); 
		    break;
		    
		case FLIP_NE_SW: 
		    unrotBuf = unrotTile->getBufferPtr(i)
			+ x * unrotTile->getLineWidth() 
			+ y * unrotTile->getPixelSize() 
			+ unrotTile->getByteOffset();
		    break;
		    
		case FLIP_NW_SE:
		    unrotBuf = unrotTile->getBufferPtr(i)
			+ (width-x-1) * unrotTile->getLineWidth()
			+ (height-(y+1)) * unrotTile->getPixelSize()
			+ unrotTile->getByteOffset();
		    break;
		    
		case ROTATE_FULL:
		    unrotBuf = unrotTile->getBufferPtr(i)
			+ (height-y-1) * unrotTile->getLineWidth()
			+ (width-x-1) * unrotTile->getPixelSize()
			+ unrotTile->getByteOffset();
		    break;
		}
		
		// use memcpy for support of all pixel types 
		memcpy(rotBuf, unrotBuf, pixSize);
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////
//   transforms the rotated x to unrotated x
////////////////////////////////////////////////////////////////////////
int RotatedImageData::getUnrotTile_x (int x)
{ 
    switch(_rotationMode) { 
      case ROTATE_NO:   return(x);
      case ROTATE_CCW:  return(getNumbLines() - x - 1);
      case ROTATE_CW:   return(x);
      case FLIP_NW_SE:  return(getNumbLines() - x - 1);
      case FLIP_NE_SW:  return(x);
      case ROTATE_FULL: return(getNumbSamples() - x - 1);
    }
    return(x);		// shouldn't happen (make compiler happy)
}

////////////////////////////////////////////////////////////////////////
//    transforms the rotated y to unrotated y 
////////////////////////////////////////////////////////////////////////
int RotatedImageData::getUnrotTile_y (int y)
{
    switch(_rotationMode) { 
      case ROTATE_NO:   return(y);
      case ROTATE_CCW:  return(y);
      case ROTATE_CW:   return(getNumbSamples() - y - 1);
      case FLIP_NW_SE:  return(getNumbSamples() - y - 1); 
      case FLIP_NE_SW:  return(y); 
      case ROTATE_FULL: return(getNumbLines() - y - 1);
    }
    return(y);		// shouldn't happen
}

////////////////////////////////////////////////////////////////////////
//   transforms the rotated x to unrotated x
////////////////////////////////////////////////////////////////////////
int RotatedImageData::getUnrotTile_x (double x)
{
    switch(_rotationMode) { 
      case ROTATE_NO:   return((int)x);
      case ROTATE_CCW:  return((int)(getNumbLines() - x - 1));
      case ROTATE_CW:   return((int)x);
      case FLIP_NW_SE:  return((int)(getNumbLines() - x - 1));
      case FLIP_NE_SW:  return((int)x); 
      case ROTATE_FULL: return((int)(getNumbSamples() - x - 1));
    }
    return((int)x);           // shouldn't happen
}
 
////////////////////////////////////////////////////////////////////////
//    transforms the rotated y to unrotated y
////////////////////////////////////////////////////////////////////////
int RotatedImageData::getUnrotTile_y (double y)
{
    switch(_rotationMode) { 
      case ROTATE_NO:   return((int)y);
      case ROTATE_CCW:  return((int)y);
      case ROTATE_CW:   return((int)(getNumbSamples() - y - 1));
      case FLIP_NW_SE:  return((int)(getNumbSamples() - y - 1));
      case FLIP_NE_SW:  return((int)y);
      case ROTATE_FULL: return((int)(getNumbLines() - y - 1));
    }
    return((int)y);           // shouldn't happen
}

////////////////////////////////////////////////////////////////////////
// Sets a new RotationMode
////////////////////////////////////////////////////////////////////////
void RotatedImageData::setRotationMode(RotationType rotMode)
{
    _rotationMode = rotMode;
    updateViews();
}

////////////////////////////////////////////////////////////////////////
// transforms display coordinates (x, y) into image coordinates
// (Line, Sample)
////////////////////////////////////////////////////////////////////////
void RotatedImageData::transDisplayToImageCoords(int x, int y, 
                                                 int *Sample, int *Line)
{
    switch(_rotationMode) { 
      case ROTATE_NO:   *Line   = y + 1;
                        *Sample = x + 1;
                        break;

      case ROTATE_FULL: *Line   = getNumbLines() - y;
                        *Sample = getNumbSamples() - x;
                        break;

      case ROTATE_CW:   *Line   = getNumbSamples() - x;
                        *Sample = y + 1;
                        break;

      case ROTATE_CCW:  *Line   = x + 1;
                        *Sample = getNumbLines() - y;
                        break;

      case FLIP_NE_SW:  *Line   = x + 1;
                        *Sample = y + 1;
                        break;

      case FLIP_NW_SE:  *Line   = getNumbSamples() - x;
                        *Sample = getNumbLines() - y;
                        break;
    }
}

////////////////////////////////////////////////////////////////////////
// transforms display coordinates (x, y) into image coordinates
// (Line, Sample)
////////////////////////////////////////////////////////////////////////
void RotatedImageData::transDisplayToImageCoords(double x, double y,
                                                 double *sample, double *line)
{
    switch(_rotationMode) { 
      case ROTATE_NO:
	  *line   = y + 1.0;
	  *sample = x + 1.0;
	  break;
      
      case ROTATE_FULL: 
	  *line   = (double)getNumbLines() - y;
	  *sample = (double)getNumbSamples() - x;
	  break;
      
      case ROTATE_CW:
	  *line   = (double)getNumbSamples() - x;
	  *sample = y + 1.0;
	  break;
	  
      case ROTATE_CCW:
	  *line   = x + 1.0;
	  *sample = (double)getNumbLines() - y;
	  break;
      
      case FLIP_NE_SW:
	  *line   = x + 1.0;
	  *sample = y + 1.0;
	  break;
	  
      case FLIP_NW_SE:
	  *line   = (double)getNumbSamples() - x;
	  *sample = (double)getNumbLines() - y;
	  break;
    }
}

////////////////////////////////////////////////////////////////
// transforms image coordinates (line, sample) into image coordinates
// (x, y)
//      Image coordinate system (0,0) => Display coordinate system (1,1)
////////////////////////////////////////////////////////////////
void RotatedImageData::transImageToDisplayCoords(int samp, int line, 
						 int *x, int *y)
{
    switch(_rotationMode) {
      case ROTATE_NO:
	  *y = line - 1;
	  *x = samp - 1;
	  break;
	  
      case ROTATE_FULL:
	  *y = getNumbLines() - line;
	  *x = getNumbSamples() - samp;
	  break;
	  
      case ROTATE_CW:
	  *x = getNumbSamples() - line;
	  *y = samp - 1;
	  break;
	  
      case ROTATE_CCW:
	  *x = line - 1;
	  *y = getNumbLines() - samp;
	  break;
	  
      case FLIP_NE_SW:
	  *x = line - 1;
	  *y = samp - 1;
	  break;
	  
      case FLIP_NW_SE:
	  *x = getNumbSamples() - line;
	  *y = getNumbLines() - samp;
	  break;
    }
}
 
////////////////////////////////////////////////////////////////
// transforms image coordinates (line, sample) into image coordinates
// (x, y)
//      Image coordinate system (0,0) => Display coordinate system (1,1)
////////////////////////////////////////////////////////////////
void RotatedImageData::transImageToDisplayCoords(double samp, double line,
						 double *x, double *y)
{
    switch(_rotationMode) {
      case ROTATE_NO:
	  *y = line - 1.0;
	  *x = samp - 1.0;
	  break;
	  
      case ROTATE_FULL:
	  *y = (double)getNumbLines() - line;
	  *x = (double)getNumbSamples() - samp;
	  break;
 
      case ROTATE_CW:
	  *x = (double)getNumbSamples() - line;
	  *y = samp - 1.0;
	  break;
 
      case ROTATE_CCW:
	  *x = line - 1.0;
	  *y = (double)getNumbLines() - samp;
	  break;
 
      case FLIP_NE_SW:
	  *x = line - 1.0;
	  *y = samp - 1.0;
	  break;
 
      case FLIP_NW_SE:
	  *x = (double)getNumbSamples() - line;
	  *y = (double)getNumbLines() - samp;
	  break;
    }
}

////////////////////////////////////////////////////////////////
// Overrides of various base class functions.  Mostly a matter of
// swapping x/y and w/h.
////////////////////////////////////////////////////////////////

StatusType RotatedImageData::getSuggestedUnzoomedTileSize(int &height, 
							  int &width)
{
    if (ROT_NO_SWAP(_rotationMode))
	return ImageDataWrapper::getSuggestedUnzoomedTileSize(height, width);
    else
	return ImageDataWrapper::getSuggestedUnzoomedTileSize(width, height);
}

void RotatedImageData::getUnzoomedTileSize(int &height, int &width)
{
    if (ROT_NO_SWAP(_rotationMode))
	ImageDataWrapper::getUnzoomedTileSize(height, width);
    else
	ImageDataWrapper::getUnzoomedTileSize(width, height);
}

void RotatedImageData::setUnzoomedTileSize(int height, int width)
{
    if (ROT_NO_SWAP(_rotationMode))
	ImageDataWrapper::setUnzoomedTileSize(height, width);
    else
	ImageDataWrapper::setUnzoomedTileSize(width, height);
}

int RotatedImageData::getNumbSamples() const
{
    if (ROT_NO_SWAP(_rotationMode))
	return ImageDataWrapper::getNumbSamples();
    else
	return ImageDataWrapper::getNumbLines();
}

int RotatedImageData::getNumbLines() const
{
    if (ROT_NO_SWAP(_rotationMode))
	return ImageDataWrapper::getNumbLines();
    else
	return ImageDataWrapper::getNumbSamples();
}

int RotatedImageData::lineSampToLatLon(double line, double samp,
		     double *lat, double *lon, int type)
{
    double x, y;
    switch(_rotationMode) { 
      case ROTATE_NO:
	x = samp;
	y = line;
	break;
      case ROTATE_FULL:
	x = getUnrotTile_x(samp);
	y = getUnrotTile_y(line);
	break;
      default:
	x = getUnrotTile_x(line);
	y = getUnrotTile_y(samp);
    }
    return ImageDataWrapper::lineSampToLatLon(y, x, lat, lon, type);
}

int RotatedImageData::latLonToLineSamp(double *line, double *samp,
		     double lat, double lon, int type)
{
    double x, y;
    int status = ImageDataWrapper::latLonToLineSamp(&y, &x, lat, lon, type);

    switch (_rotationMode) {
      case ROTATE_CCW:
	*samp = y;
	*line = (getNumbSamples() - x - 1);
 	break;
      case ROTATE_CW:
	*samp = (getNumbLines() - y - 1);
	*line = x;
	break;
      case FLIP_NW_SE:
	*samp = (getNumbLines() - x - 1);
	*line = (getNumbSamples() - y - 1);
	break;
      case FLIP_NE_SW:
	*samp = y;
	*line = x;
	break;
      case ROTATE_FULL:
	*samp = (getNumbSamples() - x - 1);
	*line = (getNumbLines() - y - 1);
	break;
      default:		// really case ROTATE_NO:
	*samp = x;
	*line = y;
	break;
    }
    return status;
}

////////////////////////////////////////////////////////////////
//  calculates a new TileZoom for the rotated image  
////////////////////////////////////////////////////////////////
ZoomFactor &RotatedImageData::calcTileZoom(ImageTile *tile,
					   int Xin, int Xout, int Yin,
					   int Yout, int Xpan, int Ypan)
{ 
    ZoomFactor urZoom;
    static ZoomFactor rZoom;

    if (ROT_NO_SWAP(_rotationMode))
	return ImageDataWrapper::calcTileZoom(tile, Xin, Xout, Yin, Yout,
					      Xpan, Ypan);
    
    urZoom = ImageDataWrapper::calcTileZoom(tile, Yin, Yout, Xin, Xout,
					    Ypan, Xpan);

    // Now the hard part... we have to swap the zoom object back, and tell
    // the tile that's what it really wants.

    rZoom.setX(urZoom.getYIn(), urZoom.getYOut());
    rZoom.setY(urZoom.getXIn(), urZoom.getXOut());
    rZoom.setSubPixelPanX(urZoom.getSubPixelPanY());
    rZoom.setSubPixelPanY(urZoom.getSubPixelPanX());

    if (tile != NULL) {  
	tile->setTileZoom(rZoom.getXIn(), rZoom.getXOut(), 
			  rZoom.getYIn(), rZoom.getYOut());
	tile->setTileSubPixelPan(rZoom.getSubPixelPanX(), 
				 rZoom.getSubPixelPanY());
    }
    return rZoom;
}

////////////////////////////////////////////////////////////////
// Create a tile.  Note that this needs to be a *rotated* tile!
// TileZoom should already be in rotated coords.  A rare case where
// we don't want to call ImageDataWrapper's routine!
////////////////////////////////////////////////////////////////

ImageTile &RotatedImageData::createTile(ZoomFactor &tileZoom, Boolean)
{
    StatusType status;
    int h, w;

    ImageTile *tile = new ImageTile;
    if (!tile) {
	status = imFAILURE;
	return *tile;	//!!!! will crash?!
    }

    initTileProperties(*tile, tileZoom);	// inits to unrotated size
    getUnzoomedTileSize(h, w);			// will swap if necessary
    tile->setSize(h, w);

    status = tile->createBuffers();
    if (status == imFAILURE)
	delete tile;	//!!!! will probably crash?!

    return *tile;
}

//////////////////////////////////////////////////////////////////////////////
//    reads one pixel from the rotated image by using the unrotated function 
//////////////////////////////////////////////////////////////////////////////
StatusType RotatedImageData::readPixel( ColorType color, int x, int y,
                                        unsigned char * bufferPtr)
{ 
    switch(_rotationMode) { 
    case ROTATE_NO:
	return ImageDataWrapper::readPixel(color, x, y, bufferPtr );
	
    case ROTATE_FULL:
	return ImageDataWrapper::readPixel(color, getUnrotTile_x(x),
					  getUnrotTile_y(y),
					  bufferPtr );
	
    default:
	return ImageDataWrapper::readPixel(color, getUnrotTile_x(y),
					  getUnrotTile_y(x),
					  bufferPtr );
    }
}

//////////////////////////////////////////////////////////////////////////////
//  reads one line from the rotated image 
//////////////////////////////////////////////////////////////////////////////
StatusType RotatedImageData::readLine(ColorType color, int line,
                                      unsigned char * bufferPtr)
{ 
    int x;
    unsigned char *ptr;
    StatusType  status = imFAILURE;
    
    if(_rotationMode == ROTATE_NO)
	return ImageDataWrapper::readLine(color, line, bufferPtr);
    
    ptr=bufferPtr;
    for(x=0; x<getNumbSamples(); x++, ptr += _pixelType.getPixelSize())
	status = readPixel( color, x, line, ptr);
    return (status);
}

//////////////////////////////////////////////////////////////////////////////
//  reads one tile from the rotated image 
//////////////////////////////////////////////////////////////////////////////
StatusType RotatedImageData::readTile(int unzoomedStartSample,
                                      int unzoomedStartLine,
                                      int unzoomedWidth,
                                      int unzoomedHeight,
                                      ImageTile &tile)
{ 
    int ss, sl, w, h;                // transformed coords of the unrotated 
    int zw, zh, zss, zes, zsl, zel;  // tile with and without zooms  
    StatusType status;
    ImageTile *unrotTile;
    ZoomFactor zoom;

    // When ROTATE_NO is selected function from unrotated image will be called 
    if (_rotationMode == ROTATE_NO) { 
	status = ImageDataWrapper::readTile(unzoomedStartSample,
					   unzoomedStartLine,
					   unzoomedWidth,
					   unzoomedHeight,
					   tile);
	return(status);
    }

    // Transformation from rotated to unrotated image
    ss = unzoomedStartSample;
    sl = unzoomedStartLine;
    w  = unzoomedWidth;
    h  = unzoomedHeight;
    getUnrotTileCoords(&ss, &sl, &w, &h);

    int XIn, XOut, YIn, YOut, XPan, YPan;
    
    // Get TileZoom from the rotated Image  
    zoom =tile.getTileZoom(); 

    // Swap the zoom values if neccassary 
    if(_rotationMode != ROTATE_FULL) {
	XIn=zoom.getXIn();
	XOut =zoom.getXOut();
	YIn=zoom.getYIn();
	YOut=zoom.getYOut();
	XPan=zoom.getSubPixelPanX();
	YPan=zoom.getSubPixelPanY();
	zoom.setX(YIn, YOut);
	zoom.setY(XIn, XOut);
	zoom.setSubPixelPanX(YPan);
	zoom.setSubPixelPanY(XPan);
    }

    // Create a new tile for the unrotated image 
    // Make sure this is an unrotated tile!!!
    unrotTile = & ImageDataWrapper::createTile(zoom);

    // To correct the right tile size - somebody might have changed the
    // tile size from the suggested one (e.g. ImageDisplayView).

    if (ROT_NO_SWAP(_rotationMode)) {
	if (unrotTile->getHeight() != tile.getHeight() ||
	      unrotTile->getWidth() != tile.getWidth()) {
	    unrotTile->setSize(tile.getHeight(), tile.getWidth());
	    unrotTile->createBuffers();		// re-create the buffers
	}
    }
    else {
	if (unrotTile->getHeight() != tile.getWidth() ||
	      unrotTile->getWidth() != tile.getHeight()) {
	    unrotTile->setSize(tile.getWidth(), tile.getHeight());
	    unrotTile->createBuffers();		// re-create the buffers
	}
    }
    
    // Read the needed Tile from the unrotated image 
    status = ImageDataWrapper::readTile(ss, sl, w, h, *unrotTile); 

    // get the right width and height of the image tile inside the
    // tile buffer, normally is valid:
    //       buffer width  == image tile width 
    //       buffer height == image tile height 
    // but if there is a rest that don't fill the whole tile buffer
    // you must compute the number of image pixel in the buffer
    // in reference with the actual zoom factor    
    zoom = tile.getTileZoom();
    if (unzoomedWidth == tile.getWidth())
	zw = tile.getBufferWidth();
    else {
	zss = unzoomedStartSample*zoom.getXIn()/zoom.getXOut(); 
	zes = (unzoomedStartSample+unzoomedWidth-1)*zoom.getXIn()/zoom.getXOut();
	zw = zes-zss+1;
    }
    if (unzoomedHeight == tile.getHeight())
	zh = tile.getBufferHeight();
    else {
	zsl = unzoomedStartLine*zoom.getYIn()/zoom.getYOut();
	zel = (unzoomedStartLine+unzoomedHeight-1)*zoom.getYIn()/zoom.getYOut();
	zh  = zel-zsl+1;
    }
    
    // Copy the content of unrotated tile buffer into rotated tile buffer 
    copyTileBuffer(&tile, unrotTile, zh, zw);

    // To get the right _startLine and _startSample for the rotated tile 
    tile.setStartLineSample(unzoomedStartLine, unzoomedStartSample);

    // the unrotTile will not used longer, we have to free memory !!!!!
    delete unrotTile;
    
    return (status);
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiRotateImageCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// SIRotateImageCmd.cc: Rotate image,
////////////////////////////////////////////////////////////
#include "SiRotateImageCmd.h"
#include "RotatedImageData.h"
#include "ErrorManager.h"
#include <stdint.h>

SiRotateImageCmd::SiRotateImageCmd(const char *name, int active,
		CmdList *radioList,
		RotatedImageData *image, RotationType rotation)
	: RadioCmd(name, active, radioList)
{
    _image = image;
    _rotation = rotation;

    // Set the default value

    int value = (int) (_rotation == _image->getRotationMode());
    _value = (CmdValue) (uintptr_t) value;
    newValue();
}

void SiRotateImageCmd::doit()
{
    if (_value) {
	if (_image)
	    _image->setRotationMode(_rotation);
	else 
	    theErrorManager->process(Error, "Change Rotation", 
			"You are trying to rotate an undefined image"); 
    }
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rotate_image.imake
#define SUBROUTINE rotate_image
#define MODULE_LIST RotatedImageData.cc SiRotateImageCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_RTL
#define LIB_TAE
#define LIB_MOTIFAPP
#define LIB_MOTIF

$ Return
$!#############################################################################
