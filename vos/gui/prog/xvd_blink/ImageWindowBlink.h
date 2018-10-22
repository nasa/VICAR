////////////////////////////////////////////////////////////////
// ImageWindowBlink.h
////////////////////////////////////////////////////////////////
#ifndef IMAGEWINDOWBLINK_H
#define IMAGEWINDOWBLINK_H

#include "ImageWindow.h"

class ImageWindowBlink : public ImageWindow {

protected:
	static Widget _shell, _blinker;

public:

	ImageWindowBlink( const char * name ) : ImageWindow(name) { };

	virtual void initialize();
	virtual void manage();
	virtual void unmanage();
	virtual void iconify();
	virtual void deiconify();

	static Widget getShell() { return _shell; }

};
#endif
