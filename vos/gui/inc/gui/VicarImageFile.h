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

