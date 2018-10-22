///////////////////////////////////////////////////////////////////
// TpImageInfo.h: This component displays image filename and
// image number.
///////////////////////////////////////////////////////////////////
#include "TpImageInfo.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <stdio.h>

TpImageInfo::TpImageInfo(Widget parent, const char *name, 
		char *filename, int number)
	: UIComponent(name)
{
    _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, 
			  NULL);
    installDestroyHandler();

    char buf[16];
    sprintf(buf, "Image %d", number);
    XmString numStr = XmStringCreateLocalized (buf);
 
    _imageNumber = XtVaCreateManagedWidget("imageNumber",
                            xmLabelWidgetClass, _w,
                            XmNlabelString, numStr,
                            XmNtopAttachment, XmATTACH_FORM,
                            XmNleftAttachment, XmATTACH_FORM,
                            XmNrightAttachment, XmATTACH_NONE,
                            XmNbottomAttachment, XmATTACH_FORM,
                            NULL);
    
    XmString fileStr = XmStringCreateLocalized(filename);
    XtVaCreateManagedWidget("filename",
                            xmLabelWidgetClass, _w,
                            XmNlabelString, fileStr,
                            XmNalignment, XmALIGNMENT_END,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, _imageNumber, 
			    XmNleftOffset, 10,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNbottomAttachment, XmATTACH_FORM,
                            NULL);
}

void TpImageInfo::setNumber(int n) 
{
    char buf[16];
    sprintf(buf, "Image %d", n);
    XmString numStr = XmStringCreateLocalized (buf);

    XtVaSetValues(_imageNumber, XmNlabelString, numStr, NULL);

    XmStringFree(numStr);
}
