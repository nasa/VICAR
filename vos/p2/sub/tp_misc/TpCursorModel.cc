////////////////////////////////////////////////////////////////
// TpCursorModel.cc: Same as CursorModel, only it sets tracking
// on not just one, but on an array of n imaging widgets.
////////////////////////////////////////////////////////////////
#include "TpCursorModel.h"
#include "CursorBasicView.h"
#include "XvicImage.h"

TpCursorModel::TpCursorModel(Boolean trackingEnabled, 
		Widget iw, Widget *aiw, int n) 
	: CursorModel(trackingEnabled, iw)
{
    _aiw = aiw;
    _n = n;

    // SET RESOURCE IN WIDGET TO TRACK 
    for (int i = 0; i < _n; i++)
	XtVaSetValues(_aiw[i], XvicNtrackFloatingCursor, True, NULL);
}

TpCursorModel::~TpCursorModel()
{
    for (int i = 0; i < _n; i++)
	XtVaSetValues(_aiw[i], XvicNtrackFloatingCursor, False, NULL);
}

void TpCursorModel::getImageWidgets(Widget *&aiw, int &n)
{
    n = _n;
    aiw = new Widget [_n];
    for (int i = 0; i < _n; i++)
	aiw[i] = _aiw[i];
}
