////////////////////////////////////////////////////////////
// PseudoRGBController.h: This class derives from RGBController
//                        The only difference is that it changes
//                        not only ColorModel, but also
//                        PseudoValue for the current mark as 
//                        user drags the slider
/////////////////////////////////////////////////////////////
#ifndef PSEUDORGBCONTROLLER_H
#define PSEUDORGBCONTROLLER_H
#include "RGBController.h"

class PseudoMarks;

class PseudoRGBController : public RGBController {

  protected:
    
    PseudoMarks *_pseudoMarks;

    virtual void  redChanged ( int );
    virtual void  greenChanged ( int );
    virtual void  blueChanged ( int );
    
  public:
    
    PseudoRGBController ( Widget , ColorModel *, const char *, PseudoMarks * );

    const char *const className() { return "PseudoRGBController"; }
};
#endif
