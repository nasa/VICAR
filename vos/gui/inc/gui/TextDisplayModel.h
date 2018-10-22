///////////////////////////////////////////////////////////////////
//
//    TextDisplayModel.h: 
//
//    This is the model class for text display .
//
///////////////////////////////////////////////////////////////////
#ifndef TEXTDISPLAYMODEL_H
#define TEXTDISPLAYMODEL_H

typedef enum _TextStyle { NORMAL, HIGHLIGHT } TextStyle;

class ImageData;

class TextDisplayView;

class TextDisplayModel {

  protected:
    
    ImageData *_image;
    char *_text;
    
    int _numViews;
    TextDisplayView **_views;
    
    void updateViews();
    void updateViews(char *newText, int size, TextStyle style);
    
  public:
    
    TextDisplayModel(ImageData *);
    virtual ~TextDisplayModel();
    
    void attachView(TextDisplayView *); // Add dependent view object
    void detachView(TextDisplayView *); // Remove dependent view object

    void setText(char *, int, TextStyle = NORMAL);
    char *getText() { return _text; };
    ImageData *getImage() { return _image; };
    void clear();

    virtual const char *const className () { return "TextDisplayModel"; }
};
#endif
