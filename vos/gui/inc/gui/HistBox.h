////////////////////////////////////////////////////////////////
// HistBox.h: Container class for histogram view components.
////////////////////////////////////////////////////////////////
#ifndef HISTBOX_H
#define HISTBOX_H
#include "UIComponent.h"
#include "HistDefs.h"
#include "Histogram.h"

class HistAxisView;
class HistGraphView;
class StatView;

class HistBox : public UIComponent {

  private:

	static int _histBoxInit;           // flag for class initialization

	Boolean _shell_resize;

	static XtResource _resources[];

	static Boolean CvtStringToPopupDirectionType(Display *,
                   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );
	static Boolean CvtStringToMethodType(Display *,
                   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );
	static Boolean CvtStringToOrientType(Display *,
                   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );
	static Boolean CvtStringToVerAxisDirType(Display *,
		   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );

  protected:

	Histogram 		*_histR;
	Histogram 		*_histG;
	Histogram 		*_histB;

	HistBox 		*_histBoxR;
	HistBox 		*_histBoxG;
	HistBox 		*_histBoxB;

	Widget 			_sepRG, _sepGB;

	HistAxisView 		*_histHorAxisView;
	HistAxisView 		*_histVerAxisView;
	HistGraphView 		*_histGraphView;
	StatView 		*_statView;

	PopupDirectionType 	_popDirection;  // Meaningfull only for "Pop-up
	MethodType 		_method;
	OrientType 		_histOrient;
	VerAxisDirType		_verAxisDir;

	static String 		_defaults[];

        Boolean 		_showAxis;
        Boolean 		_showHist;
        Boolean 		_showStat;

        void layComponents();   // Shows only specified components
        void showComponents();  // Manages components
        void hideComponents();  // Unmanages components

  public:

	HistBox(Widget, const char *,
			Histogram *, Histogram * =NULL, Histogram* =NULL);
	~HistBox();

        void setPopupDirectionType ( PopupDirectionType );
        void setMethodType ( MethodType );
        void setOrientType ( OrientType );
	void setVerAxisDirType ( VerAxisDirType );
	void setSpike (int spike);

	PopupDirectionType getPopupDirectionType() { return _popDirection; }
	MethodType getMethodType() { return _method; }
	OrientType getOrientType() { return _histOrient; }
	VerAxisDirType getVerAxisDirType() { return _verAxisDir; }
        int getSpike();

	void setLogScale(Boolean log);

	void showAxis (Boolean show);
	void showHist (Boolean show);
	void showStat (Boolean show);

	Boolean AxisIsDisplayed () { return _showAxis; }
	Boolean HistIsDisplayed () { return _showHist; }
	Boolean StatIsDisplayed () { return _showStat; }
	Boolean logScaleIsSet ();

	Histogram *getHistR () { return _histR; }
	Histogram *getHistG () { return _histG; }
	Histogram *getHistB () { return _histB; }

	virtual const char *const className() { return "HistBox"; }
};
#endif
