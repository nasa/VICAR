//////////////////////////////////////////////////////////////////////////////
// TpDisplayer.cc: This class manages multiple subdisplayer windows, of which
// at most three can be displayed on the screen at one time.
//////////////////////////////////////////////////////////////////////////////
#ifndef TPDISPLAYER_H
#define TPDISPLAYER_H
#include "UIComponent.h"
#include "TpDefs.h"
#include "RotationDefs.h"

class TpSubDisplayer;
class TpImageReference;
class TpMatchManager;
class TpMatch;

class TpDisplayer : public UIComponent {

  private:

    static XtResource _resources[];

    String _cursor;
    String _cursorColor;

    Widget _form;

    TpMatchManager *_matchManager;
    TpImageReference *_imageReference;
    TpSubDisplayer *_image[TP_MAX_IMAGES];
    
    // A maximum of TP_MAX_DISPLAYS images can be displayed at one time
 
    TpSubDisplayer *_displayed[TP_MAX_DISPLAYS];

    int _nimages;	// Total in memory
    int _numWin;	// Visible on screen
    int _win[TP_MAX_DISPLAYS];	// In this order
    Boolean _locks[TP_MAX_DISPLAYS];	// Some may be locked during shiftings

    void layoutComponents() const;
    void showComponents() const;
    void hideComponents() const;

  public:

    TpDisplayer(Widget parent, const char *name, 
		TpMatchManager *, TpImageReference *);
    ~TpDisplayer();

    void reload(TpDisplayer *);

    String getCursor() { return _cursor; }
    void setCursor(String);

    String getCursorColor() { return _cursorColor; }
    void setCursorColor(String);

    int addImage(char *filename);
    int deleteImage(int n);

    int getNumImages() { return _nimages; }
    TpSubDisplayer *getImage(int i) { return _image[i]; }

    void setNumWin(int numWin);
    int getNumWin() { return _numWin; }

    void shiftLeft();
    void shiftRight();
    void setLock(int image);
    void unSetLock(int image);
    void setNumDisplays(int);
    TpSubDisplayer *getSubDisplayer(int i) { return _image[i]; }
    void newMatchSelected(TpMatch *);

    virtual const char * const className() { return ("TpDisplayer"); } 
};
#endif
