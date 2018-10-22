//////////////////////////////////////////////////////////////
// StretchParmInpView.h: A component class to show keyin fields
/////////////////////////////////////////////////////////////
#ifndef STRETCHPARMINPVIEW_H
#define STRETCHPARMINPVIEW_H
#include "KeyinView.h"
#include "StretchValue.h"

class StretchCmdInterface;

enum StretchParmType { StretchInt, StretchDouble, StretchString };

class StretchParmInpView : public KeyinView {

  private:

    static StretchValue _defaultStretchValue;	// holds default values

  protected:

    StretchValue *_stretchValue;
    int _offset;
    StretchParmType _type;
    StretchCmdInterface *_stretchCmdInterface;

  public:

    StretchParmInpView ( Widget, const char *, StretchValue *, int,
			 StretchParmType, StretchCmdInterface * );

    virtual void update( XtPointer = NULL );
    virtual void setValue(StretchValue *);
    virtual void setForeground(char *color);

    virtual const char *const className() { return "StretchParmInpView"; }
};
#endif

