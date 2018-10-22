/////////////////////////////////////////////////////////////
// SiHistSetRowCmd.cc:  Arrange r,g,b histograms in row order.
/////////////////////////////////////////////////////////////
#include "SiHistSetRowCmd.h"
#include "SiHistBox.h"

SiHistSetRowCmd::SiHistSetRowCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;

    if ( ( _box->getPopupDirectionType() == ROW &&
	   _box->getMethodType() == POPUP ) ||
	_box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

	newValue();
}

void SiHistSetRowCmd::doit()
{
    if (_value)
	_box->setPopupDirectionType ( ROW );
}

void SiHistSetRowCmd::undoit()
{
//    Boolean value = _box->getPopupDirectionType();
//
  //  if ( value ) 
//	_box->setPopupDirectionType ( COLUMN );
  //  else
//	_box->setPopupDirectionType ( ROW );
//
   // _value = (CmdValue)!value;
  //  newValue();
}
