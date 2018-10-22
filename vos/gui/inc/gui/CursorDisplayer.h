///////////////////////////////////////////////////////////////////
//  CursorDisplayer.h:
//
//	Instantiates all cursor views, model and maybe 
//	someday a controller.   
//
///////////////////////////////////////////////////////////////////
#ifndef CURSORDISPLAYER_H
#define CURSORDISPLAYER_H
#include "UIComponent.h"

class CursorDnView;
class CursorPositionView;

class CursorLatLonView;

class CursorModel;
class ImageData;
class BasicImageView;

class CursorDisplayer : public UIComponent {


private:

	CursorDnView *		_cursorDnView;     // to display DN values
	CursorPositionView *	_cursorPositionView;  //  x-y cursor coords

        CursorLatLonView *      _cursorLatLonView;

	CursorModel *		_cursorModel;	      // model
	
protected:

        	static String _defaults[]; // resource defaults.

public:

	CursorDisplayer( Widget parent, char *name,  ImageData *dataModel,
			 BasicImageView *imageView  );
	virtual ~CursorDisplayer();

	virtual const char * const className() { return ("cursorDisplayer"); } 

};
#endif

