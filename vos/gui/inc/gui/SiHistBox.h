////////////////////////////////////////////////////////////////
// SiHistBox.h: Container class for histogram view components.
////////////////////////////////////////////////////////////////
#ifndef SiHistBOX_H
#define SiHistBOX_H
#include "SiHistSingleBox.h"

class SiHistBox : public SiHistSingleBox {

  private:

    static int _histBoxInit;           // flag for class initialization

    static XtResource _resources[];

  protected:

    SiHistogram 		*_histG;
    SiHistogram 		*_histB;

    SiHistSingleBox	*_histBoxR;
    SiHistSingleBox	*_histBoxG;
    SiHistSingleBox	*_histBoxB;

    Widget 		_sepRG, _sepGB;

    PopupDirectionType 	_popDirection;  // Meaningfull only for Pop-up
    MethodType 		_method;

    void layComponentsRow();
    void layComponentsCol();

  public:

    SiHistBox ( Widget, const char *,
				SiHistogram *, SiHistogram *, SiHistogram * );
    virtual ~SiHistBox();

    virtual void setHistBox();

    virtual void layComponents();       // Shows only specified components
    virtual void showComponents();      // Manages components
    virtual void hideComponents();      // Unmanages components

    void setOrientType ( OrientType );
    void setVerAxisDirType ( VerAxisDirType );
    void setSpike ( int spike );
    void setLogScale ( Boolean log );
    void setPopupDirectionType ( PopupDirectionType );
    void setMethodType ( MethodType );

    PopupDirectionType getPopupDirectionType() { return _popDirection; }
    MethodType getMethodType() { return _method; }

    virtual void showAxis (Boolean show);
    virtual void showStat (Boolean show);

    SiHistogram *getHistG() { return _histG; }
    SiHistogram *getHistB() { return _histB; }

    virtual const char *const className() { return "SiHistBox"; }
};
#endif
