//////////////////////////////////////////////////////////////
// DnValueView.cc: A display for numeric position value on bw scale(0-255)
///////////////////////////////////////////////////////////////
#include "DnValueView.h"
#include "KeyinView.h"
#include "ColorModel.h"
#include "stdio.h"
#include <Xm/RowColumn.h>

DnValueView::DnValueView (Widget parent, const char *name) : ColorView (name)
{
  _w = XtCreateWidget(_name, 
		xmRowColumnWidgetClass, parent, 
		NULL, 0 );
  installDestroyHandler();

  _position = new KeyinView(_w, name);
  _position->manage();
  _position->setFieldValue((char *)"0");
}

void DnValueView::update (ColorModel *color)
{
  char buf[100];
  sprintf(buf, "%3.3d", color->red());
  _position->setFieldValue ( buf );
}
