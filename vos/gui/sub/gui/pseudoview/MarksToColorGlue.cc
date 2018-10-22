///////////////////////////////////////////////////////
// MarksToColorGlue.cc:
///////////////////////////////////////////////////////
#include "MarksToColorGlue.h"
#include "PseudoMarks.h"
#include "ColorModel.h"

void MarksToColorGlue::update(PseudoMarks *marks)
{
	_colorModel->setRgb(marks->getRed(), marks->getGrn(), marks->getBlu());
	_bwModel->setRgb(marks->getDn(), marks->getDn(), marks->getDn());
}
