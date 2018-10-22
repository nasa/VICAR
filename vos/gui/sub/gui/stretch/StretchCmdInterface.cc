//////////////////////////////////////////////////////////////
// StretchCmdInterface.cc: Command interface subclass that fills
// StretchValue structure end calls execution on the command with
// filled-in value.
//////////////////////////////////////////////////////////////
#include "StretchCmdInterface.h"
#include "StretchRadioBtn.h"
#include "StretchCheckBox.h"
#include "StretchParmInpView.h"
#include "PostDialogCmd.h"
#include "StretchParmListDialog.h"
#include "RadioCmdBox.h"
#include "ButtonInterface.h"
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <iostream>
using namespace std;

XtResource StretchCmdInterface::_resources[] = {
 {
   (char *)"alarmValue", 
   (char *)"AlarmValue",
   XmRInt,
   sizeof ( int ),
   XtOffsetOf ( StretchCmdInterface, _alarmValue ),
   XmRImmediate,
   ( XtPointer ) 255,
 },
 {
   (char *)"unselectedParmColor", 
   (char *)"UnselectedParmColor", 
   XmRString,
   sizeof ( String ),
   XtOffset ( StretchCmdInterface *, _unselectedParmColor ),
   XmRString,
   ( XtPointer ) "black",
 },
 {
   (char *)"selectedParmColor", 
   (char *)"SelectedParmColor", 
   XmRString,
   sizeof ( String ),
   XtOffset ( StretchCmdInterface *, _selectedParmColor ),
   XmRString,
   ( XtPointer ) "blue",
 },
};

//////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////
StretchCmdInterface::StretchCmdInterface ( Widget parent, Cmd *cmd )
    : CmdInterface ( cmd )
{
    int i;

    _w = XtVaCreateWidget(_name,
			  xmFormWidgetClass, parent,
			  NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.

    if ( _active )
	activate();
    else
	deactivate();      

    Widget stretchForm = XtVaCreateManagedWidget("stretchForm",
                xmFormWidgetClass, _w,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
                NULL );

    _numRadios = 0;
    _numChecks = 0;
    _numInpViews = 0;
    
    // Get current value

    _stretchValue = new StretchValue (*((StretchValue *)(_cmd->getValue())));

    /////////////////////////////////////////////////////////////////
    // Stretch type names are listed in a radio bank

    Widget stretchNameRC = XtVaCreateManagedWidget("stretchNameRC",
                xmRowColumnWidgetClass, stretchForm,
                XmNorientation, XmVERTICAL,
		XmNradioBehavior, True,
		XmNradioAlwaysOne, True,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_NONE,
		XmNbottomAttachment, XmATTACH_NONE,
                NULL);

    // If you add to this list make sure you adjust Stretch_MAX_RADIOS!!

    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"clip", CLIP, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"contour", CONTOUR, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"ellipse", ELLIPSE, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"func", FUNCTION, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"gauss", GAUSS, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"itable", ITABLE, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"linear", LINEAR, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"log", LOGARITHM, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"percent", ASTRETCH, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"period", PSTRETCH, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"power", POWER, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
                "raw", RAW, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"smooth", SMOOTH, this);

    for (i = 0; i < _numRadios; i++ )
	_radioBtnList[i]->manage();


    /////////////////////////////////////////////////////////////////////

    Widget stretchParms1 = XtVaCreateManagedWidget("stretchParms1",
                xmFormWidgetClass, stretchForm,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, stretchNameRC,
                NULL );

    // If you add to this list make sure you adjust Stretch_MAX_INP_VIEWS!!
    // The indexes in the table are significant.  If you change anything which
    // changes the table indexing, you must also change the activation lists
    // in setValue().
    
    // Keyin single-value parameters, first column

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 0
		"low", _stretchValue,
		XtOffsetOf(StretchValue, low), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 1
		"high", _stretchValue,
		XtOffsetOf(StretchValue, high), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
		XmNtopWidget,	    _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 2
		"dnmin", _stretchValue,
		XtOffsetOf(StretchValue, dnmin), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 3
		"dnmax", _stretchValue,
		XtOffsetOf(StretchValue, dnmax), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 4
		"dnValue", _stretchValue,
		XtOffsetOf(StretchValue, dnValue), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 5
		"gmean", _stretchValue,
		XtOffsetOf(StretchValue, gmean), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 6
		"gsigma", _stretchValue,
		XtOffsetOf(StretchValue, gsigma), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 7
		"nbits", _stretchValue,
		XtOffsetOf(StretchValue, nbits), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 8
                "curve", _stretchValue,
                XtOffsetOf(StretchValue, curve), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 9
                "interval", _stretchValue,
                XtOffsetOf(StretchValue, interval), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    // Second column of parameters

    Widget stretchParms2 = XtVaCreateManagedWidget("stretchParms2",
                xmFormWidgetClass, stretchForm,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, stretchParms1,
		XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 10
                "lPerc", _stretchValue,
                XtOffsetOf(StretchValue, lPerc), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment, XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 11
                "hPerc", _stretchValue,
                XtOffsetOf(StretchValue, hPerc), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 12
		"maxnum", _stretchValue,
		XtOffsetOf(StretchValue, maxnum), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 13
		"backgnd", _stretchValue,
		XtOffsetOf(StretchValue, backgnd), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 14
		"mean", _stretchValue,
		XtOffsetOf(StretchValue, mean), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 15
		"pmean", _stretchValue,
		XtOffsetOf(StretchValue, pmean), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 16
		"ampl", _stretchValue,
		XtOffsetOf(StretchValue, ampl), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 17
		"freq", _stretchValue,
		XtOffsetOf(StretchValue, freq), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 18
		"phi", _stretchValue,
		XtOffsetOf(StretchValue, phi), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    ///////////////////////////////////////////////////////////////////////
    // ITable Parameter 

    _itableListDialog = new StretchParmListDialog("itableListDialog", 
						  _cmd, ITABLE);
    Cmd *postITableListCmd;
    postITableListCmd = new PostDialogCmd("PostITableListDialog", 
					  True, _itableListDialog );
    _itableValueList = new ButtonInterface(stretchParms2, postITableListCmd);
    XtVaSetValues ( _itableValueList->baseWidget(),
		XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-1]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );
    _itableValueList->manage();

    /////////////////////////////////////////////////////////////
    // Function parameter

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchForm, // 19
                "func", _stretchValue,
                XtOffsetOf(StretchValue, func), StretchString, this);
    XtVaSetValues(_inpViewList[_numInpViews-1]->baseWidget(),
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, stretchParms2,
		XmNleftAttachment, XmATTACH_NONE,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
		NULL);

    /////////////////////////////////////////////////////////////
    // Manage all keyin parameters

    for (i = 0; i < _numInpViews; i++ )
	_inpViewList[i]->manage();

    /////////////////////////////////////////////////////////////
    // Post Stretches

    Widget postFrame =  XtVaCreateManagedWidget("postFrame",
		xmFrameWidgetClass, _w,
		XmNtopAttachment, XmATTACH_WIDGET,
                XmNtopWidget, stretchForm,
                XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL );
    XtVaCreateManagedWidget ("postLabel",
		xmLabelGadgetClass, postFrame,
		XmNchildType, XmFRAME_TITLE_CHILD,
		XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
		NULL );

    Widget postForm =  XtVaCreateManagedWidget("postForm",
                xmFormWidgetClass, postFrame,
                NULL );

    Widget rcPostList = XtVaCreateManagedWidget("rcPostList",
                xmRowColumnWidgetClass, postForm,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
                NULL);

    // Post stretches are implemented as checkboxes

    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "table", 
						      TABLE, this);
    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "alarm", 
						      ALARM, this);
    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "comp", 
						      COMP, this);
    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "off",
                                                      OFF, this);

    // Manage all the checkboxes

    for (i = 0; i < _numChecks; i++ )
	_checkBoxList[i]->manage();


    // Table Stretch Values

    _tableListDialog = new StretchParmListDialog ("tableListDialog", 
						  _cmd, TABLE );
    Cmd *postTableListCmd = new PostDialogCmd("PostTableListDialog", 
					      True, _tableListDialog);
    CmdInterface *tableValueList = new ButtonInterface(postForm, 
						       postTableListCmd);
    XtVaSetValues ( tableValueList->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, rcPostList,
		NULL );
    tableValueList->manage();

    // Alarm Stretch Values

    _alarmListDialog = new StretchParmListDialog ("alarmListDialog", 
						  _cmd, ALARM, _alarmValue );
    Cmd *postAlarmListCmd;
    postAlarmListCmd = new PostDialogCmd("PostAlarmListDialog", True, 
					 _alarmListDialog);
    CmdInterface *alarmValueList;
    alarmValueList = new ButtonInterface(postForm, postAlarmListCmd);
    XtVaSetValues ( alarmValueList->baseWidget(),
                XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, tableValueList->baseWidget(),
                XmNleftAttachment, XmATTACH_WIDGET,
                XmNleftWidget, rcPostList,
                NULL );
    alarmValueList->manage();

    setValue(_cmd->getValue());
}

//////////////////////////////////////////////////////////////
// Actually run the command by creating a dynamically allocated
// copy of the stretchValue (because that's what Cmd likes).
//////////////////////////////////////////////////////////////
void StretchCmdInterface::stretchIt(StretchValue *stretchValue)
{
    _stretchValue = stretchValue;
    runCmd((CmdValue *) new StretchValue(*_stretchValue));
}

void StretchCmdInterface::stretchIt()
{
    runCmd((CmdValue *) new StretchValue(*_stretchValue));
}

//////////////////////////////////////////////////////////////
// Update the interface to match a given value
//////////////////////////////////////////////////////////////
void StretchCmdInterface::setValue(CmdValue value)
{
    int i;

    CmdInterface::setValue(value);	// Removes cmd from deferred list

    if (value != NULL)		// NULL means use saved default
	*_stretchValue = *((StretchValue *)value);
    else 
	cerr << "StretchCmdInterface::setValue: No Value error\n";

    for (i = 0; i < _numInpViews; i++)
	_inpViewList[i]->setValue(_stretchValue);

    //_tableListDialog->setValue(_stretchValue);
    //_alarmListDialog->setValue(_stretchValue);

    for (i = 0; i < _numChecks; i++) {
	if (_checkBoxList[i]->getType() == TABLE)
	    _checkBoxList[i]->setValue(_stretchValue->table);
	else if (_checkBoxList[i]->getType() == ALARM)
	    _checkBoxList[i]->setValue(_stretchValue->alarm);
	else if (_checkBoxList[i]->getType() == COMP)
	    _checkBoxList[i]->setValue(_stretchValue->comple);
	else if (_checkBoxList[i]->getType() == OFF)
            _checkBoxList[i]->setValue(_stretchValue->off);
    }

    for (i = 0; i < _numRadios; i++)
	_radioBtnList[i]->setValue(_stretchValue->stretchName);

    setSensitivity(_stretchValue->stretchName);
}

//////////////////////////////////////////////////////////////
// We set sensitivity for relevant parameters by first setting them all
// to false, then setting the relevant ones to true, based on
// stretch type.
//////////////////////////////////////////////////////////////
void StretchCmdInterface::setSensitivity(StretchType type)
{
    Boolean sens_flag[Stretch_MAX_INP_VIEWS];
    
    // Check for post-stretches
    
    if ( (type == COMP) || (type == ALARM) || (type == TABLE) )
	return;
    
    int i;
    for (i = 0; i < _numInpViews; i++)
	sens_flag[i] = False;
    
    XtSetSensitive(_itableValueList->baseWidget(), False);
    XtVaSetValues(_itableValueList->baseWidget(),
		  XtVaTypedArg,
		      XmNforeground, XmRString, 
		      _unselectedParmColor, (strlen(_unselectedParmColor) + 1),
		  NULL);

    switch (type) {
	
    case LINEAR:
	sens_flag[0] = True;					// low
	sens_flag[1] = True;					// high
	break;
	
    case RAW:
	break;

    case OFF:
	break;

    case CLIP:
	sens_flag[7] = True;					// nbits
	break;
	
    case CONTOUR:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[4] = True;					// dnValue
	sens_flag[9] = True;					// interval
	sens_flag[12] = True;					// maxnum
	//!!!! stretched??
	break;
	
    case ITABLE:
	sens_flag[13] = True;					// backgnd
	XtSetSensitive(_itableValueList->baseWidget(), True);  // inTable, outTable
	XtVaSetValues(_itableValueList->baseWidget(),
		      XtVaTypedArg,
                          XmNforeground, XmRString, 
		          _selectedParmColor, (strlen(_selectedParmColor) + 1),
		      NULL);

	break;
	
    case PSTRETCH:
	sens_flag[15] = True;					// pmean
	sens_flag[16] = True;					// ampl
	sens_flag[17] = True;					// freq
	sens_flag[18] = True;					// phi
	break;
	
    case FUNCTION:
	sens_flag[19] = True;					// func
	break;
	
    case LOGARITHM:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[8] = True;					// curve
	break;
	
    case ASTRETCH:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[10] = True;					// lPerc
	sens_flag[11] = True;					// hPerc
	break;
	
    case GAUSS:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[5] = True;					// gmean
	sens_flag[6] = True;					// gsigma
	break;
	
    case SMOOTH:
    case ELLIPSE:
    case POWER:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	break;
    default:
	break;			// shouldn't happen, but fairly benign
    }

    for (i = 0; i < _numInpViews; i++) {
	XtSetSensitive(_inpViewList[i]->baseWidget(), sens_flag[i]);
	sens_flag[i] ? _inpViewList[i]->setForeground(_selectedParmColor) : 
	               _inpViewList[i]->setForeground(_unselectedParmColor);
    }
}
