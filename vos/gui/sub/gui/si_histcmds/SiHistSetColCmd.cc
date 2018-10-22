/////////////////////////////////////////////////////////////
// SiHistSetColCmd.cc:  Arrange r,g,b histograms in column order.
/////////////////////////////////////////////////////////////
#include "SiHistSetColCmd.h"
#include "SiHistBox.h"

SiHistSetColCmd::SiHistSetColCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;

    if ( ( _box->getPopupDirectionType() == COLUMN &&
	   _box->getMethodType() == POPUP ) ||
	 _box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

	newValue();
}

void SiHistSetColCmd::doit()
{
    if (_value)
       _box->setPopupDirectionType ( COLUMN );
}

void SiHistSetColCmd::undoit()
{
//    Boolean value = _box->getPopupDirectionType();
//
  //  if ( value ) 
    //   _box->setPopupDirectionType ( ROW );
//    else
  //      _box->setPopupDirectionType ( COLUMN );
//
  //  _value = (CmdValue)!value;
    //newValue();
}

