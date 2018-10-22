////////////////////////////////////////////////////////////////
// MagInfo.h: Displays mag text information on magnification rate 
// and mag size.
///////////////////////////////////////////////////////////////
#ifndef MAGINFO_H
#define MAGINFO_H
#include "UIComponent.h"

class KeyinView;

class MagInfo : public UIComponent {

    protected:

	static String _defaults[];

	KeyinView *_magSize, *_magRatio;

    public:

	MagInfo(Widget parent, const char *name);
	virtual ~MagInfo();

	void printSize(int w, int h);
	void printRatio(float ratioX, float ratioY);

	virtual const char *const className() { return "MagInfo"; } 
};
#endif
