//////////////////////////////////////////////////////////////
// SgAxisView.h: A component class to show a plot axis.
/////////////////////////////////////////////////////////////
#ifndef SGAXISVIEW_H
#define SGAXISVIEW_H
#include "UIComponent.h"

class SgAxisView : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    static String _defaults[];

    // Attribures that can change at run-time
    float _min;
    float _max;
    Boolean _intRange;

    GC _gc;
    XFontStruct *_fontStruct;
    char *_fontname;
    char *_drawColor;

    // Geometry
    Dimension _height, _width;
    Boolean _vertical;
    Boolean _ascending;
    Boolean _ticksOnly;
    Dimension _drawOffset, _fistTickMargin, _lastTickMargin;
    Dimension _longTickLength, _shortTickLength;
    Dimension _twoTicks, _fourTicks, _eightTicks;
    Dimension _tickThickness;
    Dimension _strOffset;

    static void displayCallback ( Widget, XtPointer, XtPointer);
    virtual void display();

    virtual void drawHorizontal ( Dimension width );
    virtual void drawVertical ( Dimension height, Dimension width );
    virtual int getNumTicks ( Dimension );

  public:

    SgAxisView ( Widget, const char *, Boolean vertical = TRUE );
    virtual ~SgAxisView();

    void setLimits ( int min, int max );
    void setLimits ( float min, float max );

    void setIntRange ( Boolean intRange );

    void setVertical ( Boolean vertical );
    void setAscending ( Boolean ascending );

    virtual const char *const className() { return "SgAxisView"; }
};
#endif

