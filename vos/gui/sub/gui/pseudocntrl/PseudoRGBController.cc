////////////////////////////////////////////////////////////
// PseudoRGBController.cc: This class derives from RGBController
//			   The only difference is that it changes 
//			   not only ColorModel, but also
//			   PseudoMarks value for the 
//			   current mark as user drags the slider
/////////////////////////////////////////////////////////////
#include "PseudoRGBController.h"
#include "PseudoMarks.h"
#include "ColorModel.h"

PseudoRGBController::PseudoRGBController ( Widget parent, 
			 ColorModel *model, const char *name,
			 PseudoMarks *marks )
			  : RGBController ( parent, model, name )
{
    _pseudoMarks = marks;
}

void PseudoRGBController::redChanged ( int value )
{
    _model->setRed ( value );
    _pseudoMarks->setRed ( value );
}

void PseudoRGBController::greenChanged ( int value )
{
    _model->setGreen ( value );
    _pseudoMarks->setGrn ( value );
}

void PseudoRGBController::blueChanged ( int value )
{
    _model->setBlue ( value );
    _pseudoMarks->setBlu ( value );
}
