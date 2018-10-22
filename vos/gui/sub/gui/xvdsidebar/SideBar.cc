///////////////////////////////////////////////////////
// SideBar.cc:
////////////////////////////////////////////////////////
#include "SideBar.h"
#include "BasicImageView.h"
#include "ImageDisplayView.h"
#include "SiHistBtnInterface.h"
#include "LutBtnInterface.h"
#include "SiHistMenuCmd.h"
#include "ImageWindow.h"
#include "CursorModel.h"
#include "CursorDnView.h"
#include "CursorPositionView.h"
#include "ZoomCmdSet.h"
#include "OptionCmdMenu.h"
#include "MagInfo.h"
#include "ImageSizeView.h"
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>

SideBar::SideBar ( Widget parent, const char * name, BasicImageView *imageView,
		ImageData *imageData, Cmd *histCmd,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
		Cmd *lutCmd, Lut *lutR, Lut *lutG, Lut *lutB,
		ZoomCmdSet *zoomCmdSet)
	: UIComponent (name)
{
	// CREATE SIDE BAR TOOLBOX FOR<

	_w  = XtVaCreateWidget ( _name,
                                xmFormWidgetClass,
                                parent,
				NULL );
	installDestroyHandler ();
	
	// CREATE CURSOR MODEL

	CursorModel *cursorModel = new CursorModel( True, imageView->getWidget() );

	// DISPLAY CURSOR POSITION

	unsigned char bitFlags = (unsigned char) 255;

	_cursorPositionView = new CursorPositionView( _w, "cursorPositionView", 
			  cursorModel, imageData,  bitFlags );
	XtVaSetValues(_cursorPositionView->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			NULL);

	// DISPLAY CURSOR DN VALUE 

	_cursorDnView = new CursorDnView( _w, "cursorDnView", 
					cursorModel,  imageData,  bitFlags );
	XtVaSetValues(_cursorDnView->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _cursorPositionView->baseWidget(),
			NULL);

	// Put separator between cursor stuff and zoom

	Widget sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, _w,
			NULL, 0);
	XtVaSetValues(sep1,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _cursorDnView->baseWidget(),
			NULL);

	// DISPLAY NUMBER OF LINES AND SAMPLES

	_imageSizeView = new ImageSizeView( _w, "imageSizeView", 
					imageData);
	XtVaSetValues(_imageSizeView->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, sep1,
			NULL);

	// Put separator between cursor stuff and zoom

	Widget sep2 = XtCreateManagedWidget("sep2", xmSeparatorWidgetClass, _w,
			NULL, 0);
	XtVaSetValues(sep2,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _imageSizeView->baseWidget(),
			NULL);

	// Display zoom option menu

	OptionCmdMenu *zoomMenu = new OptionCmdMenu(_w, "zoomMenu",
				zoomCmdSet->getRadioList());
	XtVaSetValues(zoomMenu->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, sep2,
			NULL);

	// Display histogram "button" 

	CmdInterface *popHistBtn = new SiHistBtnInterface ( _w, histCmd,
                  histR, histG, histB );
	XtVaSetValues(popHistBtn->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, zoomMenu->baseWidget(),
			NULL);

	// Display LUT "button"

	CmdInterface *lutBtn = new LutBtnInterface ( _w, lutCmd,
		  lutR, lutG, lutB );
	XtVaSetValues(lutBtn->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, popHistBtn->baseWidget(),
			NULL);

	_magInfo = new MagInfo ( _w, "magInfo" );
	XtVaSetValues(_magInfo->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, lutBtn->baseWidget(),
			NULL);


	_cursorPositionView->manage();
	_cursorDnView->manage();
  	_imageSizeView->manage();
	zoomMenu->manage();
	popHistBtn->manage();
        lutBtn->manage();
}

