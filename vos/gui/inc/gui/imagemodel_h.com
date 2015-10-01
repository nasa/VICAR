$!****************************************************************************
$!
$! Build proc for MIPL module imagemodel_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:31
$!
$! Execute by entering:		$ @imagemodel_h
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
$ write sys$output "*** module imagemodel_h ***"
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
$ write sys$output "Invalid argument given to imagemodel_h.com file -- ", primary
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
$   if F$SEARCH("imagemodel_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @imagemodel_h.bld "STD"
$   else
$      @imagemodel_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create imagemodel_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack imagemodel_h.com -mixed -
	-s ImageData.h ImageDefs.h ImageTile.h VicarImageData.h -
	   VicarImageFile.h ZoomFactor.h ImagePixelType.h ImageDataWrapper.h -
	   ImageLabel.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ImageData.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageData.h
//
//	This is an abstract class for image data. Currently  
//	(Oct 94) the only subclass available is VicarImageData
//	data which is the standard MIPL Vicar formatted
//	 (1 or 3) files.  This is used with ImageView for
//	displaying an image from file onto screen.
//
//	"this describes what an image is, not what it
//	looks like or how to draw it"
//
//
//   FUTURE ENHANCEMENTS:
//	(1) Allow Mode to be multispectrum
////////////////////////////////////////////////////////////////
#ifndef IMAGEDATA_H
#define IMAGEDATA_H
#include "ImageDefs.h"
#include "ZoomFactor.h"
#include "ImageTile.h"
#include "ImagePixelType.h"
#include "ImageLabel.h"
#include <Xm/Xm.h>              // only for Boolean!

#include <iostream>

const int MAX_CHANS = 3;	// Max # of channels (e.g. R,G,B)
const int MAX_FILES = 3;	// Max # of physical files

typedef enum LongitudeDirection { UNDEFdirection,EAST, WEST}LongitudeDirection;

class BasicImageView;
class ImageLabel;

class ImageData {

 protected:

   ImageLabel *_labelTree;      // The root of the label tree.
   char _lastKey[MAX_IMAGE_LABEL_KEY_SIZE];

   int			_numViews;
   BasicImageView	**_views;

   int _numberOfLines;
   int _numberOfSamples;

   int _tileHeight;
   int _tileWidth;

   ModeType	_mode;			// bw, color, etc
   ImagePixelType _pixelType;

   char	_inputDataSourceName[4000];  // usually a filename
   Boolean _dataSourceOpened;

   Boolean _inconsistentImage;
   Boolean _errorMsgIssued;	// So we don't do multiples
   char	*_errorMsg;

   double _fileMinDataRange;
   double _fileMaxDataRange;
   double _userMinDataRange;
   double _userMaxDataRange;

   Boolean _fileDataRangeValid;
   Boolean _useFileMin;
   Boolean _useFileMax;

   ZoomFactor _newTileZoom; // really just a temp variable to hold space

   virtual void updateViews();	// Called whenever the model's data changes
   virtual void updatePartViews(int flags); // only part of the model changed

   virtual void initTileProperties(ImageTile &tile, ZoomFactor &tileZoom);

   virtual void setErrorMsg(const char *);  // For subclasses to report errors

   // Must be called by all open()'s that support more than byte data.
   virtual void initDataRange();

   // Determine data range from the file.  Should be overridden by any
   // subclass that has data range info easily available (e.g. from a label
   // instead of reading all the data).
   virtual void readDataRange();
   virtual void findMaxMinInLine(unsigned char *buffer);

   int _mapProjInfoPresent;  // 1 if we have map projection data

   ImageData();

 public:

   virtual ~ImageData();

   // Label stuff
   virtual StatusType getLabelSetValue(char*& label, char *key, int *maxsize) 
   { return imSUCCESS; }
   ImageLabel *getLabelRoot() { return _labelTree; }

   // View stuff
   virtual void attachView(BasicImageView * view);
   virtual void detachView(BasicImageView * view);

   virtual void getInputDataSourceName(char *name) 
	{ strcpy(name, _inputDataSourceName); } 
   virtual char *getInputDataSourceName() { return _inputDataSourceName; }
   // Used for reload only... in case a subclass munges the name somehow
   virtual char *getReloadDataSourceName() { return _inputDataSourceName; }
   virtual Boolean isDataSourceOpened() const { return _dataSourceOpened; }

   // Image data management
   virtual StatusType open(char *) = 0;		// open file(s)
   virtual StatusType close() = 0;		// close files(s)

   // To get information only
   virtual int getPixelSize() { return _pixelType.getPixelSize(); }
   virtual void getPixelType(char *pixelType) const
	{ strcpy(pixelType, _pixelType.getString()); }
   virtual ImagePixelType getPixelType() const { return _pixelType; }

   virtual ModeType getMode() const { return _mode; }	// color or black&white
   virtual int getNumbSamples() const { return  _numberOfSamples; }
   virtual int getNumbLines() const { return  _numberOfLines; }

   // Get optimum buff size for reading in data
   virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);

   // Get numb lines and samples per image
   virtual void getUnzoomedTileSize(int &height, int &width)
	{ height = _tileHeight; width = _tileWidth; }

   // Transformation of display coords into image coords
   virtual void transDisplayToImageCoords(int x, int y, int *samp, int *line);
   virtual void transDisplayToImageCoords(double x, double y, 
                                          double *samp, double *line);

   // Transformation of image coords into display coords
   virtual void transImageToDisplayCoords(int samp, int line, int *x, int *y);
   virtual void transImageToDisplayCoords(double samp, double line, 
                                          double *x, double *y);

   // Tile/buffer management.  Subclasses may override any of these.
   virtual ImageTile &createTile(ZoomFactor &tileZoom,
				 Boolean willCreateBuffers = TRUE);
   virtual void destroyBuffers();	// destroy buffers owned by the model
   virtual Boolean isNewBufferNeeded(ImageTile &tile)
	{ return tile.isNewBufferNeeded(); }

   // Outside controls, defaults if not called

   // Set size of tile (for reading in data)
   virtual void setUnzoomedTileSize(int h, int w)
	{ if (h != 0) _tileHeight = h; if (w != 0) _tileWidth = w; }

   // Set mode: color or black&white. will default if not set
   virtual void setMode(ModeType mode)
	{ _mode = mode; updateViews(); }

   // Data good or bad 
   virtual Boolean isConsistent() const { return (Boolean) !_inconsistentImage; }

   // Get tile (partial) zoom on image
   virtual ZoomFactor &getTileZoom(ImageTile &tile) { return tile.getTileZoom();}

   // Get and set data range
   virtual void getDataRange(double &min, double &max);
   virtual double getMinDataRange()
	{  double min,max; getDataRange(min,max); return min;  }
   virtual double getMaxDataRange()
	{  double min,max; getDataRange(min,max); return max;  }
   virtual void setDataRange(double min, double max);
   virtual void setDataMin(double min);
   virtual void setDataMax(double max);

   // True if min/max comes from file instead of user
   virtual Boolean isMinAuto() { return _useFileMin; }
   virtual Boolean isMaxAuto() { return _useFileMax; }
   virtual void setMinAuto(Boolean min_auto);
   virtual void setMaxAuto(Boolean max_auto);

   // Read image data
   virtual StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
				int unzoomedWidth, int unzoomedHeight,
				ImageTile & tile ) = 0;
   virtual StatusType readLine(ColorType color, int line,
				unsigned char *bufferPtr) = 0;
   virtual StatusType readPixel(ColorType color, int sampleOffset,
				int lineOffset, unsigned char *bufferPtr) = 0;

   // Zoom
   virtual ZoomFactor &calcTileZoom(ImageTile *tile,
		int Xin=1, int Xout=1, int Yin=1, int Yout=1,
		int Xpan=0, int Ypan=0);

   // Error management
   virtual char *getErrorMsg() { _errorMsgIssued=True; return _errorMsg; }
   virtual Boolean errorMsgIssued() { return _errorMsgIssued; }

   static ImageType getImageTypeFromFile( char * );

   // derived classes should implement the MP methods, if applicable:

   virtual int readMapProjInfo() { return 0; }

   // for the LatLon functions, the "type" argument refers to the map
   // projection type (0 = planetocentric, 1 = planetodetic)

   virtual int lineSampToLatLon(double   /* line */ ,
                                double   /* samp */ ,
                                double * /* lat */ ,
				double * /* lon */ ,
                                int      /* type */ ) { return 0; }

   virtual int latLonToLineSamp(double * /* line */ ,
                                double * /* samp */ ,
                                double   /* lat */ ,
                                double   /* lon */ ,
                                int      /* type */ ) { return 0; }
				
   virtual int isMapProjInfoPresent() { return _mapProjInfoPresent; }

   // for MP-capable images, determine whether EAST or WEST longitude is used:
   // 0 for undefined, 1 for east, 2 for west
   virtual LongitudeDirection getLonDirection() { return UNDEFdirection; }
   virtual char* getLastKey() { return _lastKey; }
   virtual void setLastKey(char* lastkey) { strcpy(_lastKey, lastkey); }
};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageDefs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageDefs.h
////////////////////////////////////////////////////////////////
#ifndef IMAGEDEFS_H
#define IMAGEDEFS_H

enum StatusType	{ imFAILURE, imSUCCESS };
enum ColorType { UNDEFcolor, BWcolor, RED, GREEN, BLUE };
enum ModeType { UNDEFmode, BWmode, COLORmode, ERRORmode };
enum ImageType { UNDEFimage, VICARimage, PDSimage, PDS_VICARimage };


//enum LongitudeDirection { UNDEFdirection, EAST, WEST }; // ImageDefs.h maybe?

//enum	ZoomType { ZOOMspecial, ZOOM2fit, ZOOMnone };

// Bit flags to indicate only a part of the model changed
// More should probably be added in the future
#define IMAGE_DATA_UPDATE_RANGE 0x01

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageTile.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageTile.h
//
//	Created by image view (or created by model when requested by view).
//	Keeps tile information and location of buffer(s) that are needed for
//	the imaging widget.
//
//	FUTURE ENHANCEMENTS:
//		(1)  Fill in function getPixel().  This
//		       routine is a stub for now.
//
////////////////////////////////////////////////////////////////
#ifndef IMAGETILE_H
#define IMAGETILE_H
#include "ImageDefs.h"
#include "ZoomFactor.h"
#include "ImagePixelType.h"
#include <Xm/Xm.h>	// only for Boolean!

class ImageTile {

 protected:

   ImagePixelType _pixelType;
   ZoomFactor	_tileZoom;
   int		_tileWidth;
   int		_tileHeight;
   int		_bufferSampleWidth;
   int		_bufferSampleHeight;
   int		_startLine;
   int		_startSample;
   int		_lineWidth;  // if Tile owns buffer, lineWidth = _tileWidth *
			     // _pixelSize; else it is set by the buffer owner.
   int		_bufferSize;	// units = bytes; all buffers the same size
   unsigned char * _bufferPtr[3];
   int		_numberOfBuffers;
   Boolean	_tileOwnsBuffer;
   ModeType	_mode;
   int		_byteOffset;
   Boolean	_newBufferNeeded;

   void		createBufferAttributes();

 public:

   ImageTile()
	: _numberOfBuffers(0), _tileOwnsBuffer(FALSE), _newBufferNeeded(True)
	{ } 
   virtual ~ImageTile() { destroyBuffers(); }

   // Management of buffers owned by tile.  1 - 3 buffers are needed to store
   // the image data that is read in by the model.  Buffers can be owned by:
   // (1) tile, (2) view, (3) model.  ImageDisplayView puts the ownership on
   // the tile.

   StatusType createBuffers();	// Optional.  Called if tile 'owns' buffers.
   StatusType destroyBuffers();

 	// sets buffer pointers, called by buffer owner
   void	setBuffers(unsigned char *ptr0=NULL, unsigned char *ptr1=NULL,
		unsigned char *ptr2=NULL, int lineWidth=0, int byteOffset=0);
   inline Boolean doesTileOwnBuffer() const { return _tileOwnsBuffer; }
	// change ownership of buffer to (or from) tile
   inline void changeBufferOwnerToTile(Boolean own)
	{  _tileOwnsBuffer = own; }
   inline Boolean isNewBufferNeeded() { return _newBufferNeeded; }

   // Init tile properties .. set by image subclasses

   virtual void	setTileZoom(int, int, int, int); // sets prezoom
   virtual void setTileSubPixelPan(int x, int y);
   inline void setSize(int height, int width)	// set size of tile to be read
	{  if (height != 0) _tileHeight = height;
	   if (width != 0) _tileWidth = width;
	   createBufferAttributes();
	}
   void setPixelType(char *pixelType) { _pixelType.set(pixelType); }
   void setPixelType(ImagePixelType dt) { _pixelType = dt; }

   inline void setMode(ModeType mode) { _mode = mode; }  // set color/bw mode

   // Info set by ImageData subclass when data is moved into buffer

   inline void setStartLineSample(int line, int sample)
	{  _startSample = sample; _startLine = line; }

   // Misc info may be needed by caller

   inline int getPixelSize() const { return _pixelType.getPixelSize(); }
   inline ImagePixelType getPixelType() const { return _pixelType; }
   inline ZoomFactor &getTileZoom() { return _tileZoom; } // get tile's prezoom
   inline int getBufferWidth() const { return _bufferSampleWidth; }
   inline int getBufferHeight() const { return _bufferSampleHeight; }
   inline int getBufferSize() const { return _bufferSize; } // size in bytes

   inline ModeType getMode() const { return _mode; }	// color or black&white
   inline int getHeight() const { return _tileHeight; }	// unzoomed coords
   inline int getWidth() const { return _tileWidth; }	// unzoomed coords
   inline int getLineWidth() const { return _lineWidth; } // in bytes
   inline int getByteOffset() const { return _byteOffset; } // in bytes to
							     // starting pixel
   inline int getXcoordinate() const { return _startSample; }
   inline int getYcoordinate() const { return _startLine; }
   inline unsigned char *getBufferPtr(int index)
	{  if (index < 3 && index >= 0) return _bufferPtr[index];
	    else return NULL;
	}

   // getPixel() is intended for returning a pixel in the same manner as
   // readPixel() does in the model.  However, VicarImageData model calls a
   // read on the file when readPixel() is called.  getPixel() should read
   // the pixel from the tile buffer for better performance.

   unsigned char getPixel(int x, int y);	// Stub only

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageData.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// VicarImageData.h
//
//	This is a subclass of ImageData.  It retrieves data
//	from VICAR formatted files.  
//
////////////////////////////////////////////////////////////////
#ifndef VICARIMAGEDATA_H
#define VICARIMAGEDATA_H
#include "ImageData.h"
#include "zvproto.h"

#ifndef NO_MP_ROUTINES
#include "mp_routines.h"
#else 
#define MP void*
#endif

#define MAXSTRINGSIZE   4000

class VicarImageFile;
class ZoomFactor;

enum VicarLabelType {V_LISTALL, V_DUMPALL, V_SYSTEM, V_PROPERTY, V_HISTORY};

class VicarImageData : public ImageData {

 protected:

   // INFO ABOUT EACH FILE  --- Vicar Specific

   int		_numbFiles;	// # of different physical files
   int		_numbChannels;	// # of logical channels, 3 for RGB or 1 for BW

   VicarImageFile *_files[MAX_FILES];	// ptrs to unique physical files
   VicarImageFile *_fileForChannel[MAX_CHANS];	// ptr copies for each chan
   int		_bandForChannel[MAX_CHANS];	// band for each channel

   // INIT VALUES IN FILES

   StatusType	initNumberOfLines();
   StatusType	initNumberOfSamples();
   StatusType	initPixelType();
   StatusType	initNumbers();		// inits bands,lines,samples, etc

   // FILE MANAGEMENT

   virtual StatusType addFile( char *, int);

   MP _mpObject;

 public:

   VicarImageData();
   virtual ~VicarImageData();

   // FILE ROUTINES:

   virtual StatusType open(char *);
   virtual StatusType close();	

   // INFORMATIONAL ONLY,	

   virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);

   // OUTSIDE CONTROLS

   virtual ZoomFactor &calcTileZoom(ImageTile *tile,
				int Xin=1, int Xout=1, int Yin=1, int Yout=1,
				int Xsubpan=0, int Ysubpan=0);

   // READ DATA

   virtual StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
				int unzoomedWidth, int unzoomedHeight,
				ImageTile &tile);

   virtual StatusType readLine(ColorType color, int line,
				unsigned char * bufferPtr);

   virtual StatusType readPixel(ColorType color, int sampleOffset,
				int lineOffset, unsigned char * pixelBuffer);

   // outside access to the files
   virtual VicarImageFile *getVicarFile(int index);

   virtual int readMapProjInfo();
   virtual int lineSampToLatLon(double line, double samp, double *lat,
				double *lon, int type);
   virtual int latLonToLineSamp(double *line, double *samp, double lat,
				double lon, int type);

   virtual LongitudeDirection getLonDirection();

   // Label display
   int getFileNumb() { return _numbFiles; }
   virtual StatusType getLabelSetValue(char*& label, char *key, int *maxsize);
   StatusType parseLabelKey(char *key, int *fileIndex,
                    VicarLabelType *type, char *set, int *instance);
   virtual const char *const className () { return "VicarImageData"; }
};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageFile.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// VicarImageFile.h
//
//	This is used to open and read in 1 vicar image file 
//	from disk using the Vicar RTL routines.  Used by
//	VicarImageData only.
////////////////////////////////////////////////////////////////
#ifndef VICARIMAGEFILE_H
#define VICARIMAGEFILE_H
#include "ImageData.h"
#include "defines.h"
#include "ImageDefs.h"
#include "ZoomFactor.h"
#include "ImagePixelType.h"
#include "VicarImageData.h"
#include <Xm/Xm.h>		// only for Boolean!

#ifndef NO_MP_ROUTINES
#include "mp_routines.h"
#else 
#define MP void*
#endif

enum LabelType { INT_LB, REAL_LB, STRING_LB, UNDEFINED_LB };  /* label type */

class ImageLabel;

class VicarImageFile {

 protected:

   static const int _rtlSUCCESS;
   static int	_instance;

   int	_unit;
   int	_fileIndex;

   // Label info
   ImagePixelType _pixelType;
   int	_numbPixelsInDim1;
   int	_numbPixelsInDim2;
   int	_numbPixelsInDim3;
   int	_dimensions;
   char	_fileOrgString[12];
   char	_fileType[40];
   char	_binLabelType[40];
   char	_bintFmt[12];
   char	_brealFmt[12];
   int	_recordSize;
   char	*_filename;
   int	_numbLinesInImage;
   int	_numbSamplesInImage;
   int	_numbBandsInImage;
   ZoomFactor _fileZoom;
   int	_numBytesPref;
   int	_numBytesHeader;
   char	_className[32];		// used for outputting messages with zvmessage
   Boolean _isOpened;
   char	_host[12];

   inline void initFileZoom()
	{  _fileZoom.setX(1, 1); _fileZoom.setY(1, 1); }
   
   void nopath(char *filename);    // gets the filename without path

   ImageLabel *_allList;   // all label list node
   ImageLabel *_allDump;   // all label dump node
   ImageLabel *_systemLabel;   // system label node
   ImageLabel *_propertyLabel; // property label node
   ImageLabel *_historyLabel;  // history label node
   ImageLabel *_taskLabel[MAX_TASKS];  // history task label nodes
   ImageLabel *_propLabel[MAX_TASKS];  // property task label nodes

 public:

   VicarImageFile(int);
   virtual ~VicarImageFile() { close(); }

   virtual StatusType open(char *);
   virtual StatusType readImageLabel();
   virtual StatusType close();

   // Reads 1 tiles worth of data, for 1 color band.
   virtual StatusType read1Tile1Band(int bandOffset,
	int startLineOffset, int height, int startSampleOffset, int width,
	ZoomFactor &userZoom, unsigned char *bufferPtr, int buffer_width);

   // returns the pixel type (byte, real, long, etc
   inline void getPixelType(char *pixelType) const
	{  strcpy(pixelType, _pixelType.getString()); }
   inline ImagePixelType getPixelType() { return _pixelType; }

   inline int getNumberOfLines() const { return _numbLinesInImage; }
   inline int getNumberOfSamples() const { return _numbSamplesInImage; }
   inline int getNumberOfBands() const { return _numbBandsInImage; }

   inline void getName(char *const toFileName) const
	{  strcpy(toFileName, _filename); }
   inline const char *getName() const { return _filename; }

   // get the file zoom on the file (always 1 for now)
   inline ZoomFactor &getFileZoom() { return _fileZoom; }

   // Has file opened successfully? (may not be used?)
   inline Boolean isOpened() { return _isOpened; }

   ////////
   // Access functions to metadata (labels, binary headers/prefix, etc.)
   ////////

   inline int getUnit() { return _unit; }

   inline int getPixelSize() { return _pixelType.getPixelSize(); }

   // Get the size of one line in Bytes (prefix+pixels)
   inline int getRecordSize() { return _recordSize; }

   // Binary prefixes/headers info

   inline int getPrefixSize() { return _numBytesPref; }
   inline int getHeaderSize() { return _numBytesHeader; }
   inline void getBinLabelType(char *type) { strcpy(type, _binLabelType); }
   inline void getBintFmt(char *bint) { strcpy(bint, _bintFmt); }
   inline void getBrealFmt(char *breal) { strcpy(breal, _brealFmt); }

   // Label reading routines 
   virtual LabelType readLabelItem(char *Property, char *Label,
				char *Lb_string, int *Lb_int, float *Lb_real);
   virtual StatusType readSystemLabel(char*& SysLabel, int *maxsize);
   virtual StatusType readHistoryLabel(char*& HistLabel, int *maxsize);
   virtual StatusType readPropertyLabel(char*& PropLabel, int *maxsize);

   // Reads the binary prefix of one line
   virtual StatusType readPrefix(int band, int Line, unsigned char *buf); 

   // Reads the binary file header
   virtual StatusType readHeader(int band, unsigned char *buf);
   
   virtual MP readMPInfoFromFile();

   // Label reading and display
   StatusType readVicarLabel(char*& labels, VicarLabelType type, 
                             char *set, int instance, int *maxsize);
   StatusType readHistLabel(char*& histLabel, char *taskname, int instance, int *maxsize);
   StatusType getHistLabel(char*& hLabel, char *taskname, int instance, int *maxsize);
   StatusType readPropLabel(char*& propLabel, char *propname, int instance, int *maxsize);
   StatusType getPropLabel(char*& propLabel, char *propName, int instance, int *maxsize);
   StatusType readSysLabel(char*& sysLabel, int *maxsize);
   ImageLabel *buildLabelTree(ImageData *image, int fileIndex);

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomFactor.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ZoomFactor.h
//
//	This is a simple object for storing the zoom info
//	and reducing it to its lowest common denominator.
//	Read XvicImage.doc for information about zoom factors.
//
//	FUTURE: extra logic for subpixelpanX < out
////////////////////////////////////////////////////////////////
#ifndef ZOOMFACTOR_H
#define ZOOMFACTOR_H

class ZoomFactor {

 protected:

   int	_zoomXIn;
   int	_zoomXOut;
   int	_zoomYIn;
   int	_zoomYOut;
   int	_subPixelPanX;
   int	_subPixelPanY;

   void	reduceRational(int *numer, int *denom);

 public:

   ZoomFactor()
	{  _zoomXIn = 1; _zoomYIn = 1; _zoomXOut = 1; _zoomYOut = 1;
	   _subPixelPanX = 0; _subPixelPanY = 0;
	}
   ZoomFactor(int zoomXIn, int zoomYIn, int zoomXOut, int zoomYOut,
		int subPPX=0, int subPPY=0)
	{  _zoomXIn = zoomXIn; _zoomYIn = zoomYIn;
	   _zoomXOut = zoomXOut; _zoomYOut = zoomYOut;
	   _subPixelPanX = subPPX; _subPixelPanY = subPPY;
	}
   virtual ~ZoomFactor() { }

   // Zoom only:

   inline virtual int getXIn()  const { return _zoomXIn; }
   inline virtual int getYIn()  const { return _zoomYIn; }
   inline virtual int getXOut() const { return _zoomXOut; }
   inline virtual int getYOut() const { return _zoomYOut; }
   virtual void setX(int in, int out);
   virtual void setY(int in, int out);

   // SubPixelPan only:

   inline virtual int getSubPixelPanX() const { return _subPixelPanX; }
   inline virtual int getSubPixelPanY() const { return _subPixelPanY; }
   inline void setSubPixelPanX(int x) { _subPixelPanX = x; }
   inline void setSubPixelPanY(int y) { _subPixelPanY = y; }

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImagePixelType.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImagePixelType.h
////////////////////////////////////////////////////////////////
#ifndef IMAGEPIXELTYPE_H
#define IMAGEPIXELTYPE_H

enum DataType { imBYTE=0, imHALF=1, imUHALF=2, imFULL=3, imUFULL=4,
		imREAL=5, imDOUBLE=6 };
#define imNUM_DATA_TYPES 7

// The data type defines may need to change on some platforms.  They
// must match the Image Widget.  If a platform needs different types, use
// #if's here.
 
typedef unsigned char imByte;
typedef short int imHalf;
typedef unsigned short int imUHalf;
typedef int imFull;
typedef unsigned int imUFull;
typedef float imReal;
typedef double imDouble;

class ImagePixelType {

 protected:

   static int _DTtoXiw[imNUM_DATA_TYPES];
   static const char *_DTtoStr[imNUM_DATA_TYPES];
   static int _DTtoSize[imNUM_DATA_TYPES];
   static double _DTtoMin[imNUM_DATA_TYPES];
   static double _DTtoMax[imNUM_DATA_TYPES];
   static int _DTtoWidth[imNUM_DATA_TYPES];

   DataType _pixelType;

 public:

   ImagePixelType() { _pixelType = imBYTE; }
   ImagePixelType(DataType d) { _pixelType = d; }
   ImagePixelType(const ImagePixelType &p) { _pixelType = p._pixelType; }

   void set(DataType d) { _pixelType = d; }
   void set(char *str);

   DataType get() const { return _pixelType; }
   const char *getString() const { return _DTtoStr[_pixelType]; }
   int getXiw() const { return _DTtoXiw[_pixelType]; }

   int isIntegral() const { return (_pixelType!=imREAL&&_pixelType!=imDOUBLE); }

   int getPixelSize() const { return _DTtoSize[_pixelType]; }
   double getMinDataRange() const { return _DTtoMin[_pixelType]; }
   double getMaxDataRange() const { return _DTtoMax[_pixelType]; }

   int neededWidth() const { return _DTtoWidth[_pixelType]; }
   void printPixel(void *value_ptr, char *buffer);

   friend int operator==(ImagePixelType p1, ImagePixelType p2)
      {  return (p1._pixelType == p2._pixelType); }
   friend int operator!=(ImagePixelType p1, ImagePixelType p2)
      {  return (p1._pixelType != p2._pixelType); }
   ImagePixelType& operator=(const ImagePixelType& p)
      {  _pixelType = p._pixelType;  return *this;  }

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageDataWrapper.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// ImageDataWrapper.h: This is a class derived from ImageData class.
// It is intended to maintain image data if the specific type
// (e.g.; VicarImageData or PdsImageData) is not known initially.
//////////////////////////////////////////////////////////////////////////////
#ifndef IMAGEDATAWRAPPER_H
#define IMAGEDATAWRAPPER_H
#include "ImageData.h"
#include "ImageLabel.h"
#include <Xm/Xm.h>              // only for Boolean!

class ImageDataWrapper : public ImageData { 

  protected:
		
    ImageData *_actualImage;    // the thing around which we're wrapped
    Boolean _weOwnImage;        // True if we "own" the ImageData object

    // The user class gives us the pointer to the actual image
    virtual void registerActualImage(ImageData *actualImage,
                                     Boolean weOwnImage);

    virtual void unregisterActualImage();

    virtual void initTileProperties(ImageTile &tile, ZoomFactor &tileZoom);

    char _lastKey[MAX_IMAGE_LABEL_KEY_SIZE];

    char _reloadDataSourceName[4000]; // like _inputDataSourceName but un-munged

  public:

    ImageDataWrapper();
    virtual ~ImageDataWrapper();
    
    // Get the pointer to the actual Image 
    virtual ImageData *getActualImage() { return _actualImage; }

    virtual void attachView(BasicImageView *view);
    virtual void detachView(BasicImageView *view);
    virtual int getPixelSize();
    virtual void transDisplayToImageCoords(int x, int y, int *samp, int *line);
    virtual void transDisplayToImageCoords(double x, double y, 
					   double *samp, double *line);
    virtual void transImageToDisplayCoords(int samp, int line, int *x, int *y);
    virtual void transImageToDisplayCoords(double samp, double line, 
					   double *x, double *y);
    virtual void setMode(ModeType mode) ;
    virtual void getDataRange(double &min, double &max);
    virtual void setDataRange(double min, double max);
    virtual void setDataMin(double min);
    virtual void setDataMax(double max);
    virtual void setMinAuto(Boolean minAuto);
    virtual void setMaxAuto(Boolean maxAuto);
    virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);
    virtual ImageTile &createTile(ZoomFactor &tileZoom,
				  Boolean willCreateBuffers = TRUE);
    virtual void destroyBuffers();
    virtual Boolean isNewBufferNeeded(ImageTile &tile);
    virtual char *getErrorMsg();
    virtual Boolean errorMsgIssued();
    StatusType open(char *filename);
    StatusType close();
    StatusType readPixel(ColorType color, int x, int y, 
			 unsigned char *bufferPtr);
    StatusType readLine(ColorType color, int line, 
			unsigned char *bufferPtr); 
    StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
			int unzoomedWidth, int unzoomedHeight,
			ImageTile &tile);
    virtual ZoomFactor &calcTileZoom(ImageTile *tile, int Xin, int Xout, 
				     int Yin, int Yout, int Xpan, int Ypan);
    void getInputDataSourceName(char *name);
    char *getInputDataSourceName(); 
    virtual char *getReloadDataSourceName() { return _reloadDataSourceName; }
    virtual Boolean isDataSourceOpened() const; 
    virtual void getPixelType(char *pixelType) const; 
    virtual ImagePixelType getPixelType() const; 
    virtual ModeType getMode() const;
    virtual int getNumbSamples() const;
    virtual int getNumbLines() const;
    virtual void getUnzoomedTileSize(int &height, int &width);
    virtual void setUnzoomedTileSize(int h, int w);
    virtual Boolean isConsistent() const;
    virtual ZoomFactor &getTileZoom(ImageTile &tile); 
    virtual double getMinDataRange();
    virtual double getMaxDataRange();
    virtual Boolean isMinAuto(); 
    virtual Boolean isMaxAuto(); 

    virtual int readMapProjInfo();
    virtual int lineSampToLatLon(double line, double samp, double *lat,
                                 double *lon, int type);

    virtual int latLonToLineSamp(double *line, double *samp, double lat,
				 double lon, int type);
    virtual int isMapProjInfoPresent();
    virtual LongitudeDirection getLonDirection();

    virtual StatusType getLabelSetValue(char*& label, char *key, int *maxsize);
    ImageLabel *getLabelRoot();

    virtual const char *const className() { return("ImageDataWrapper"); }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageLabel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
//
//   ImageLabel.h: 
//
//   This is a class for image label objects.
//
////////////////////////////////////////////////////////////////
#ifndef IMAGELABEL_H
#define IMAGELABEL_H

#define MAX_IMAGE_LABEL_SUBSET         250     // max # of subsets for labels
#define MAX_IMAGE_LABEL_STRING_SIZE    132
#define MAX_IMAGE_LABEL_ITEM_SIZE      6120
#define MAX_IMAGE_LABEL_KEY_SIZE       200
#define MAX_IMAGE_LABEL_SIZE           10000

class ImageData;

class ImageLabel {
   protected:
      char *_name;                     // human-friendly name for this node
      char _key[MAX_IMAGE_LABEL_KEY_SIZE+1]; // opaque identifier for ImageData 
                                       // for this set
      ImageData *_image;
      int _numbSubset;
      ImageLabel *_subsetList[MAX_IMAGE_LABEL_SUBSET]; 

   public:
      ImageLabel(ImageData *, const char *, const char *);
      virtual ~ImageLabel();
      char *getLabelName() { return _name; } 
      char *getLabelKey() { return _key; } 
      int getChildNumb() { return _numbSubset; }
      ImageLabel *getChildLabel(int);
      void addChild(ImageLabel *);
      void deleteLabelTree();

      virtual const char *const className () { return "ImageLabel"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
