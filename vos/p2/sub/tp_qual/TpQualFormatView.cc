//////////////////////////////////////////////////////////////////////////////
// TpQualFormatView.cc
//////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatView.h"
#include "TpQualFormatSingleView.h"
#include "TpQualFormatValue.h"
#include "TpQualFormatCmdInterface.h"
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <stdio.h>

TpQualFormatView::TpQualFormatView(Widget parent, const char *name, 
				   TpQualFormatCmdInterface *ci)
    : UIComponent(name)
{
    _ci = ci;

    _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, 
			  NULL);
    installDestroyHandler();

    Widget title = XtVaCreateManagedWidget("title", 
					   xmFormWidgetClass, _w,
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNleftAttachment, XmATTACH_FORM, 
					   XmNrightAttachment, XmATTACH_FORM,
					   XmNbottomAttachment, XmATTACH_NONE,
					   NULL);
    Widget titleName = XtVaCreateManagedWidget("titleName",
					 xmLabelWidgetClass, title, 
					 XmNtopAttachment, XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					 XmNleftAttachment, XmATTACH_POSITION,
					 XmNleftPosition, 0,
					 NULL);
    Widget titleType = XtVaCreateManagedWidget("titleType",
                                         xmLabelWidgetClass, title, 
					 XmNtopAttachment, XmATTACH_FORM,
                                         XmNbottomAttachment, XmATTACH_FORM,
                                         XmNleftAttachment, XmATTACH_POSITION,
                                         XmNleftPosition, 33,
                                         NULL);
    Widget titleUnit = XtVaCreateManagedWidget("titleUnit",
                                         xmLabelWidgetClass, title, 
					 XmNtopAttachment, XmATTACH_FORM,
                                         XmNbottomAttachment, XmATTACH_FORM,
                                         XmNleftAttachment, XmATTACH_POSITION,
                                         XmNleftPosition, 66,
                                         NULL);

    _formatView = XtVaCreateManagedWidget("formatView", 
					  xmRowColumnWidgetClass, _w,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, title,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNrightAttachment, XmATTACH_FORM, 
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNorientation, XmVERTICAL,
					  XmNnumColumns, 1,
					  XmNpacking, XmPACK_TIGHT,
					  NULL);
    _qualInfos = new SL_List<TpQualFormatSingleView *>;
}

void TpQualFormatView::setQuals(TpQualFormatValue *value)
{
    int i;
    int diff = value->getNumQuals() - _qualInfos->get_length();
    if (diff > 0) {
	for (i = _qualInfos->get_length(); i < value->getNumQuals(); i++) {
	    TpQualFormatSingleView *singleView;
	    singleView = new TpQualFormatSingleView(_formatView, 
						    "formatSingleView",
						    this, i);
	    singleView->manage();
	    _qualInfos->add(singleView);
	}
    }
    if (diff < 0) {
	for (i = 0; i < (-1) * diff; i++) {
	    TpQualFormatSingleView *view = _qualInfos->remove_first();
	    view->unmanage();
	    delete view;
	}
    }

    SL_ListWatch<TpQualFormatSingleView *> w;
    _qualInfos->init_scan(&w);
    TpQualFormatSingleView *view;
    i = 0;
    while((view = _qualInfos->next()) != NULL) {
	view->setNumber(i);
	view->setName(value->_info[i]._qualName, False);
	view->setUnit(value->_info[i]._qualUnit, False);
	view->setType(value->_info[i]._qualType);
	i++;
    }
}

void TpQualFormatView::setName(char *name, int n)
{
    _ci->setName(name, n);
}

void TpQualFormatView::setUnit(char *unit, int n)
{
    _ci->setUnit(unit, n);
}

void TpQualFormatView::setType(TpQualType t, int n)
{
    _ci->setType(t, n);
}

#if defined(vms) || defined(__VMS)
#pragma define_template SL_List<TpQualFormatSingleView *>
#pragma define_template SL_ListWatch<TpQualFormatSingleView *>
#endif

