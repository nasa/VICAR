///////////////////////////////////////////////////////////////////
//  ImageDisplayer.h:
///////////////////////////////////////////////////////////////////
#ifndef IMAGEDISPLAYER_H
#define IMAGEDISPLAYER_H
#include "UIComponent.h"
#include "BasicImageView.h"
#include "SideBar.h"
#include <iostream>

//#include "LatLonBar.h"
class LatLonBar;
class Cmd;
class ImageData;
class SiHistogram;
class Lut;
class ZoomCmdSet;

class ImageDisplayer : public UIComponent {

private:

	BasicImageView 		*_imageView;
	SideBar 		*_sideBar;

        static XtResource _resources[];
	
protected:

	Widget _bb;

	Widget _rightForm;        // holds _latLonBar and _imageView

        static String _defaults[];

	LatLonBar *_latLonBar;
	Boolean _showLatLonBar; 

        Boolean _showSideBar;

        void layoutComponents();
        void showComponents();
        void hideComponents();

public:

	ImageDisplayer( Widget parent, const char * name, ImageData *,
			Cmd *, SiHistogram *, SiHistogram *, SiHistogram *,
			Cmd *, Lut *, Lut *, Lut *, ZoomCmdSet *& );
	~ImageDisplayer();

	virtual inline Widget getWidget(){ return _imageView->getWidget(); };
	virtual BasicImageView *getImageView() { return _imageView; }

	SideBar *getSideBar() { return _sideBar; }
	void showSideBar( Boolean );
	Boolean IsSideBarDisplayed() { return _showSideBar; }

	LatLonBar *getLatLonBar() { return _latLonBar; }
	void showLatLonBar( Boolean );
	int IsLatLonBarDisplayed();

	virtual const char * const className() { return ("ImageDisplayer"); } 
};
#endif


