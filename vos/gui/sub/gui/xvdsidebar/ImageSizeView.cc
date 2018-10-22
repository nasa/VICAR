////////////////////////////////////////////////////////////////
// ImageSizeView.cc
//
//	Class for views containing number of lines and 
//      samples in image
//
///////////////////////////////////////////////////////////////
#include "ImageSizeView.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <string.h>
#include <stdio.h>

///////////////////////////////////////////////////////////////
//	Constructor
///////////////////////////////////////////////////////////////
ImageSizeView::ImageSizeView(Widget parent, const char *name, 
			ImageData *imageSizeModel) 
		: BasicImageView(name, imageSizeModel)
{
   // CREATE ROW/COLUMN TO HOLD THESE SIZE DISPLAYS
   _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, NULL);
   installDestroyHandler ();
	
   _form = NULL;
   _label = NULL;
   _textfield = NULL;

   // CREATE IMAGE SIZE DISPLAYS
   addNewSubView("ImageSize");

   // ATTACH VIEW  TO DATA MODEL
   _model->attachView(this);
}

///////////////////////////////////////////////////////////////
//	Destructor
///////////////////////////////////////////////////////////////
ImageSizeView::~ImageSizeView ()
{
   _model->detachView(this);

}

///////////////////////////////////////////////////////////////
//	updateValue:
//		Update the image size value.
///////////////////////////////////////////////////////////////
void ImageSizeView::updateValue(int ns, int nl)
{
   char buf[132];

   sprintf(buf, "%2dx%2d", ns, nl);

   XmTextFieldSetString (_textfield, buf);
}

///////////////////////////////////////////////////////////////
//	addNewSubView:
//		create a single 
//		display group consisting of: label and a 
//		textfield. The textfield is used to show  
//              values that are updated as new image is
//		displayed. A name is 
//              automatically created as : 
//		"label" +  <displayName>,   etc.   
///////////////////////////////////////////////////////////////
void ImageSizeView::addNewSubView(const char * displayName)
{
   char name[132];

      _form = XtVaCreateManagedWidget("form", 
                  xmFormWidgetClass, _w, NULL);

      // CREATE LABEL
      strcpy(name, "label"); 
      strcat(name, displayName);
      _label = XtVaCreateManagedWidget(name, 
                  xmLabelWidgetClass, _form, 
                  XmNtopAttachment, XmATTACH_FORM,
                  XmNleftAttachment, XmATTACH_FORM,
                  NULL ); 

      // CREATE VALUE:  ITS REALLY A TEXTFIELD
      strcpy(name, "textfield"); 
      strcat(name, displayName);
      _textfield = XtVaCreateManagedWidget(name, 
                  xmTextFieldWidgetClass, _form,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget, _label,
                  XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                  XmNleftWidget, _label,
                  XmNshadowThickness, 0,
                  XmNcolumns, 13,
                  XmNmarginHeight, 1,
                  NULL);
}

///////////////////////////////////////////////////////////////
//	update
//      display new image size
///////////////////////////////////////////////////////////////
void ImageSizeView::update()
{
   updateValue(_model->getNumbSamples(), _model->getNumbLines());
}


