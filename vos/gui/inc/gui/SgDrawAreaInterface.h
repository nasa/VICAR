//////////////////////////////////////////////////////////////
// SgDrawAreaInterface.h: A "push button" interface to a Cmd object.
// The surface of a push-button is a drawing area widget.
// This class is intended to have subclasses.  The subclass
// must provide the drawing area widget.
///////////////////////////////////////////////////////////////
#ifndef SGDRAWAREAINTERFACE_H
#define SGDRAWAREAINTERFACE_H
#include "CmdInterface.h"

class SgDrawAreaInterface : public CmdInterface {

  protected:

    Widget _drawingAreaWidget;

    SgDrawAreaInterface ( Widget, Cmd * );
    virtual ~SgDrawAreaInterface ( );

    void setDrawingAreaWidget ( Widget );

  public:
    
    virtual void executeCmd(XtPointer);

    int operator== ( const SgDrawAreaInterface & );
};
#endif
