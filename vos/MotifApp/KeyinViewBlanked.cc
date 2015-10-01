///////////////////////////////////////////////////////
// KeyinViewBlanked.cc: A component class to show keyin fields where
// the keyin field is blanked out (e.g. for a password)
///////////////////////////////////////////////////////
#include "KeyinViewBlanked.h"
#include <Xm/TextF.h>

KeyinViewBlanked::KeyinViewBlanked ( Widget parent, const char *name,
					char blank_char)
								// = ' '
	: KeyinView(parent, name)
{
    _blanked_text = strdup("");
    _blank_char = blank_char;

    XtAddCallback ( _field, XmNmodifyVerifyCallback,
			&KeyinViewBlanked::textModCallback, (XtPointer) this );
}

KeyinViewBlanked::~KeyinViewBlanked()
{
    if (_blanked_text)
        delete _blanked_text;
}

// We don't want to set the value of the field directly, so we
// copy the string to the internal buffer and output blanks.

void KeyinViewBlanked::setFieldValue(char *text) 
{ 
    if (text == NULL)
        text = (char *)"";

    // Save copy of blanked string

    if (_blanked_text)
        delete _blanked_text;
    _blanked_text = strdup(text);

    // Send equal-length blanked string to widget

    char *disp_text = strdup(text);
    for (int i=0; i<(int)strlen(text); i++)
        disp_text[i] = _blank_char;
    XmTextFieldSetString(_field, disp_text); 
    delete disp_text;
}

// You must free the result from XmTextFieldGetString(), so we simulate
// that here.

char* KeyinViewBlanked::getFieldValue() 
{ 
    char *ptr = XtMalloc(strlen(_blanked_text) + 1);
    strcpy(ptr, _blanked_text);
    return ptr;
}

///////////////////////////////////////////////////////
// Here's the magic of this routine.  Intercept characters
// to be inserted, save them in the internal buffer, and
// write blanks to the display instead.  Snarfed from O'Reilly 6A
///////////////////////////////////////////////////////

void KeyinViewBlanked::textModCallback ( Widget,
				XtPointer clientData,
                                XtPointer callData )
{
    KeyinViewBlanked *obj = ( KeyinViewBlanked * ) clientData;
    obj->textMod(callData);
}

void KeyinViewBlanked::textMod(XtPointer callData)
{
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)callData;

    if (cbs->startPos < cbs->currInsert ||
		cbs->text->ptr == NULL) {	// backspace
	cbs->endPos = strlen(_blanked_text);	// delete here to EOL
	_blanked_text[cbs->startPos] = '\0';	// terminate the string
	return;
    }

    if (cbs->text->length > 1) {
	cbs->doit = False;		// don't allow paste
	return;
    }

    // New character entered, so grab it

    char *new_text = new char[cbs->endPos + 2];	// new char + NULL

    if (_blanked_text) {
        strncpy(new_text, _blanked_text, (size_t)cbs->endPos);
	new_text[cbs->endPos] = '\0';		// strncpy for safety...
	delete _blanked_text;
    }
    else
	new_text[0] = '\0';

    _blanked_text = new_text;
    strncat(_blanked_text, cbs->text->ptr, cbs->text->length);
    _blanked_text[cbs->endPos + cbs->text->length] = '\0';

    for (int i=0; i<cbs->text->length; i++)
	cbs->text->ptr[i] = _blank_char;
}

