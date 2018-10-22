////////////////////////////////////////////////////////////////
// SiHistSingleBox.h: Container class for histogram view components.
////////////////////////////////////////////////////////////////
#ifndef SiHistSINGLEBOX_H
#define SiHistSINGLEBOX_H
#include "UIComponent.h"
#include "SiHistDefs.h"

class SiHistogram;
class SiHistAxis;
class SiHistGraph;
class SiHistStat;

class SiHistSingleBox : public UIComponent {

  private:

    static int _histBoxInit;           // flag for class initialization

    static XtResource _resources[];

  protected:

    SiHistogram 	*_histR;

    SiHistAxis 		*_axis;
    SiHistGraph 	*_graph;
    SiHistStat		*_statView;

    OrientType 		_histOrient;
    VerAxisDirType	_verAxisDir;
    Boolean		_logScale;

    static String 	_defaults[];

    Boolean 		_showAxis;
    Boolean 		_showHist;
    Boolean 		_showStat;

    Boolean 		_shell_resize;

    static Widget findShellWidget ( Widget w );
    void setShellResize ( Boolean value );
    void restoreShellResize();

  public:

    SiHistSingleBox ( Widget, const char *, SiHistogram * );
    virtual ~SiHistSingleBox();

    virtual void setHistBox();

    virtual void layComponents();       // Shows only specified components
    virtual void showComponents();      // Manages components
    virtual void hideComponents();      // Unmanages components

    virtual void setOrientType ( OrientType );
    virtual void setVerAxisDirType ( VerAxisDirType );
    virtual void setSpike ( int spike );

    OrientType getOrientType() { return _histOrient; }
    VerAxisDirType getVerAxisDirType() { return _verAxisDir; }
    int getSpike();

    virtual void setLogScale(Boolean log);
    Boolean logScaleIsSet ();

    virtual void showAxis (Boolean show);
    virtual void showStat (Boolean show);

    Boolean axisIsDisplayed () { return _showAxis; }
    Boolean histIsDisplayed () { return _showHist; }
    Boolean statIsDisplayed () { return _showStat; }

    SiHistogram *getHistR() { return _histR; }

    virtual const char *const className() { return "SiHistSingleBox"; }
};
#endif
