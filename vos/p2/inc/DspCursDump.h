////////////////////////////////////////////////////////////////
// DspCursDump.h
///////////////////////////////////////////////////////////////
#ifndef DSPCURSDUMP_H
#define DSPCURSDUMP_H
#include "XvicImage.h"
#include "ImageData.h"
#include "ImageDisplayView.h"
#include "zvproto.h"
// #include "DspFile.h"
#include "DspDefs.h"
#include <Xm/Xm.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <iostream>

class CursorModel;
class ImageData;
class DspFile;


class DspCursDump  {

  protected:
  
  	static XtResource 	_resources[];  	
  	int			_numbRows;
  	int			_numbColumns;  	
  	int			_skipRows; // height of pixels to skip
  	int			_skipColumns; // width of pixels to skip
  	ImageData *		_imageData;
  	ImageDisplayView *	_imageView;
  	unsigned char 		_bitFlags;
  	DspFile *		_file;

  	short 			_columnsResource;  // width of text widget
  	int			_midRow;
  	int			_midColumn;
  	int			_titleLines;

	unsigned char  		_pixelBuffer[132];    // buffer for holding pixel value
	char 			_blankString[132];
	char *			_logText;
	char 			_fileName[MAX_NAMESTRINGLENGTH];
	int			_pixelSize;
	int			_lengthOfPixelText;
	int			_numbLines;
	int			_numbSamples;
	Boolean			_redDnEnable,
				_greenDnEnable,
				_blueDnEnable,
				_bwDnEnable;
	
	
//	FUNCTION TO DETERMINE NUMBER OF DIGITS FOR THE MAX PIXEL VALUE OF THIS SIZE PIXEL                                              
        virtual int		maxSizeOfPixelText(int pixelSize);	

//	UPDATE DISPLAYS ON ALL CURSOR SUBVIEWS
	static void     	writeCursorValuesCallback( Widget,
                                                XtPointer clientData,
                                                XtPointer callData);
	virtual void  		writeCursorValues ( int cursorX, int cursorY) ;

//	LOCAL FUNCTION FOR GETTING A DN VALUE AND ITS STRING EQUIV.   
	virtual void 		getValueString( ColorType color,  
				int x, int y, char * newValueString );
				
	virtual void 		updateValue(char * buf, int currentColumn, int lastColumn, char * outBuf);			
		
	virtual void 		getColumnString( int x, char * newValueString, int lastColumn );
	virtual void		printDumpHeader( const char * colorText, 
						int lengthOfPixelText,
						char * outBuf,
						int x);
	virtual void		printLineNumber( int lineNumb, char * outBuf);
				
 public:
 
//	CONSTRUCTOR/DESTRUCTOR
	DspCursDump( 		Widget			parent,
				const char *		name,
				ImageData * 		imageData,
				ImageDisplayView *	imageView,
				unsigned char 		bitFlags,
				int			pixelSize) ;
			
	virtual ~DspCursDump();

	
	virtual void dump(char *);
	virtual const char *const className () { return "DspCursorDump"; }
	
};
#endif
