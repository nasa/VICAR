//////////////////////////////////////////////////////////
// ErrorData.cc: A unit indicating an error.  It represents
// the error text, the criticality of the error, and the
// source of the error.
//////////////////////////////////////////////////////////
#include "ErrorData.h"
#include "BasicComponent.h"	// for strdup() definition on VMS
#include <stdio.h>
#include <string.h>

ErrorData::ErrorData(ActionOnError action, const char *preText, 
		const char *text, const char *details) 
{
    _action = action;

    if (preText)
	_preText = strdup(preText);
    else 
	_preText = NULL;

    if (text)
	_text = strdup(text);
    else 
	_text = NULL;

    if (details)
	_details = strdup(details);
    else 
	_details = NULL;
}

ErrorData::ErrorData(const ErrorData& ed)
{
    _action = ed._action;

    if (ed._preText)
        _preText = strdup(ed._preText);
    else
        _preText = NULL;

    if (ed._text)
        _text = strdup(ed._text);
    else
        _text = NULL;

    if (ed._details)
        _details = strdup(ed._details);
    else
        _details = NULL;
}

ErrorData &ErrorData::operator=(const ErrorData& ed) 
{
    _action = ed._action;

    if (_preText)
        delete [] _preText;

    if (_text)
        delete [] _text;

    if (_details)
        delete [] _details;

    if (ed._preText)
        _preText = strdup(ed._preText);
    else
        _preText = NULL;

    if (ed._text)
        _text = strdup(ed._text);
    else
        _text = NULL;

    if (ed._details)
        _details = strdup(ed._details);
    else
        _details = NULL;

    return *this;
}


ErrorData::~ErrorData()
{
    if (_preText)
	delete [] _preText;

    if (_text)
	delete [] _text;

    if (_details)
	delete [] _details;
}

// This routine allocates and return string.  The caller must
// free the value.  

char *ErrorData::getErrorMsg()
{
    int length = 0;

    if (_preText)
	length += strlen(_preText) + 2;

    if (_text)
	length += strlen(_text);

    if (_details)
	length += strlen(_details) + 2;

    char *string = new char [length + 2];
    string[0] = '\0';

    if (_preText) {
	strcpy(string, _preText);
	strcat(string, ": ");
    }

    if (_text)
	strcat(string, _text);

    if (_details) {
	strcat(string, "(");
	strcat(string, _details);
	strcat(string, ")");
    }

    strcat(string, "\n");

    return string;
}
