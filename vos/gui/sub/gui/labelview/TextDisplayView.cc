/////////////////////////////////////////////////////////////
//
//    TextDisplayView.cc
//
//    This is a class derived from BasicImageView.
//    It is the view for text output.
//
/////////////////////////////////////////////////////////////
#include "TextDisplayView.h"
#include "UIComponent.h"
#include <Xm/Text.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

XtResource TextDisplayView::_resources[] = {
   {
      (char *)"newRunString",
      (char *)"NewRunString",
      XmRString,
      sizeof ( String ),
      XtOffset ( TextDisplayView *, _newRunString ),
      XmRString,
      ( XtPointer ) "--------------------------------------------------------------------------------\n",
   },
};

String TextDisplayView::_defaults[] = {
   (char *)"*text.editMode:			XmMULTI_LINE_EDIT",
   (char *)"*text.rows:			20",
   (char *)"*text.columns:			60",
   (char *)"*text.cursorPositionVisible:	False",
   (char *)"*text.editable:			False",
    NULL,
};

TextDisplayView::TextDisplayView(const char *name, ImageData *image)
	: BasicImageView( name, image )
{
   _textM = NULL;
   _textW = NULL;
   _clearEveryRun = True;
}

TextDisplayView::~TextDisplayView()
{
   if (_textW) XtDestroyWidget(_textW);
   if (_textM) _textM->detachView(this);
}

void TextDisplayView::update()
{
   if (_textW && _textM) {
      if (_textM->getText())
         XmTextSetString(_textW, _textM->getText());
      else 
         XmTextSetString(_textW, '\0');
	
      XmTextShowPosition(_textW, XmTextGetLastPosition(_textW));
   }
}

Widget TextDisplayView::createTextArea ( Widget parent )
{
   // Load the default resources into the database

   setDefaultResources ( parent, _defaults );
   XtGetApplicationResources(parent, (XtPointer)this,
		      _resources, XtNumber(_resources), NULL, 0);
    
   _textW = XmCreateScrolledText ( parent, (char *)"text", NULL, 0 );

   update();
    
   XtManageChild (_textW);
    
   return _textW;
}

void TextDisplayView::registerTextModel(TextDisplayModel *model)
{
   if (!model) return;
   _textM = model;
   _textM->attachView (this);
}

void TextDisplayView::unregisterTextModel()
{
   if (_textM)
      _textM->detachView(this);
   _textM = NULL;
}

void TextDisplayView::addText(char *newText, int size, TextStyle style)
{
   if (_textW) {
      if ( _clearEveryRun ) 
         XmTextSetString(_textW, '\0');

      XmTextPosition curPos = XmTextGetLastPosition(_textW);
      XmTextInsert(_textW, curPos, newText);
      XmTextPosition newCurPos = curPos + size;
      XmTextShowPosition(_textW, newCurPos);

      if (style == HIGHLIGHT)
         XmTextSetHighlight(_textW, curPos, newCurPos, 
		       XmHIGHLIGHT_SELECTED);
   }
}

