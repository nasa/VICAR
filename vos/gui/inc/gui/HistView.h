///////////////////////////////////////////////////////////////////
// HistView.h:
///////////////////////////////////////////////////////////////////
#ifndef HISTVIEW_H
#define HISTVIEW_H
#include "UIComponent.h"
#include "HistDefs.h"
#include <iostream>

class Histogram;

class HistView : public UIComponent {

  protected:

	OrientType 	_hor;
	MethodType 	_method;
	VerAxisDirType 	_verAxisDir;

	Histogram *_hist;
	Histogram *_hist1;
	Histogram *_hist2;

  public:

	HistView ( const char *name ) : UIComponent (name) { }
	virtual ~HistView() { }

	void setOrientType (OrientType hor) { _hor = hor; }
	void setMethodType (MethodType method) { _method = method; }
	void setVerAxisDir (VerAxisDirType verAxisDir) 
					{ _verAxisDir = verAxisDir; }

	virtual void update()=0;

	virtual const char *const className() { return "HistView"; }
};
#endif

