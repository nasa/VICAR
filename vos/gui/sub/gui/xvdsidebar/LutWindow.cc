////////////////////////////////////////////////////////////////////
// LutWindow.cc:
////////////////////////////////////////////////////////////////////
#include "LutWindow.h"
#include "Lut.h"
#include "LutBox.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>

LutWindow::LutWindow ( const char *name, 
		Lut *lutR, Lut *lutG, Lut *lutB ) : MainWindow ( name )
{
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}


Widget LutWindow::createWorkArea ( Widget parent )
{
   _form = XtVaCreateWidget("workArea", 
			xmFormWidgetClass, parent,
			NULL);

   _lutBox = new LutBox(_form, "lutBox", _lutR, _lutG, _lutB);

   XtVaSetValues   ( _lutBox->baseWidget(),
                     XmNtopAttachment,     XmATTACH_FORM,
                     XmNleftAttachment,    XmATTACH_FORM,
                     XmNrightAttachment,   XmATTACH_FORM,
                     XmNbottomAttachment,  XmATTACH_FORM,
                     NULL );

   _lutBox->manage();

   return (_form);
}
