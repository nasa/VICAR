///////////////////////////////////////////////////////////////////
//
//    TextDisplayModel.cc: 
//
//    This is the model class for text display .
//
///////////////////////////////////////////////////////////////////
#include "TextDisplayModel.h"
#include "TextDisplayView.h"
#include "ViewMacros.h"
#include <assert.h>

TextDisplayModel::TextDisplayModel(ImageData *imageData)
{
   _image = imageData;
   _text = NULL;
   _numViews = 0;
   _views = NULL;
}

TextDisplayModel::~TextDisplayModel()
{
   delete [] _text;
   delete [] _views;
}

void TextDisplayModel::attachView(TextDisplayView *view)
{
   AttachViewMacro(TextDisplayView, _views, _numViews, view);
   view->update();
}

void TextDisplayModel::detachView(TextDisplayView *view)
{
   DetachViewMacro(TextDisplayView, _views, _numViews, view);
}
 
void TextDisplayModel::updateViews(char *newText, int size, TextStyle style)
{
   for (int i = 0; i < _numViews; i++)
      _views[i]->addText(newText, size, style);
}

void TextDisplayModel::updateViews()
{
   for (int i = 0; i < _numViews; i++)
      _views[i]->update();
}

void TextDisplayModel::setText(char *newText, int size, TextStyle style)
{
   if (_text && (strlen(_text) > 0)) {
      char *tempText = new char [strlen(_text) + size + 1];
      strcpy(tempText, _text);
      strncat( tempText, newText, size);
      delete [] _text;
      _text = tempText;
   }
   else {
      delete [] _text;
      _text = new char[size+1];
      strncpy(_text, newText, size);
      _text[size] = '\0';
   }
    
   updateViews(newText, size, style);
}

void TextDisplayModel::clear()
{
   if (_text) {
      delete [] _text;
      _text = NULL;
   }
    
   updateViews();
}

