////////////////////////////////////////////////////////////////////
// HistWindow.cc:
////////////////////////////////////////////////////////////////////
#include "HistWindow.h"
// #include "MenuBar.h"
#include "NoOpCmd.h"
#include "SetStackNoBlendCmd.h"
#include "SetStackBlendCmd.h"
#include "SetPopUpDirRowCmd.h"
#include "SetPopUpDirColCmd.h"
#include "SetHorHistGraphCmd.h"
#include "SetVertHistGraphCmd.h"
#include "SetAscAxisCmd.h"
#include "SetDescAxisCmd.h"
#include "HistLogScaleCmd.h"
#include "SpikeDialog.h"
#include "SpikeDialogCmd.h"
#include "StatsCmd.h"
#include "AxisCmd.h"
#include "HistCmd.h"
#include "UndoCmd.h"
#include "QuitCmd.h"
#include "MenuCmdList.h"
#include "SeparatorCmd.h"
#include "PopupMenu.h"
#include "Histogram.h"
#include "HistBox.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/RepType.h>

HistWindow::HistWindow ( Histogram *histR, Histogram *histG, Histogram *histB, 
				char *name ) : MenuWindow ( name )
{
    _histR = histR;
    _histG = histG;
    _histB = histB;
}


Widget HistWindow::createWorkArea ( Widget parent )
{

    _form = XtVaCreateWidget("workArea", 
				xmFormWidgetClass, parent,
				NULL);

    _histBox = new HistBox(_form, "histBox", _histR, _histG, _histB);

    XtVaSetValues   ( _histBox->baseWidget(),
                     XmNtopAttachment,     XmATTACH_FORM,
                     XmNleftAttachment,    XmATTACH_FORM,
                     XmNrightAttachment,   XmATTACH_FORM,
                     XmNbottomAttachment,  XmATTACH_FORM,
                     NULL );
    // Create the Hist_VIEW Pulldown Menu objects
    
    CmdList *radList0 = new CmdList();
    _stackNoBlend = new SetStackNoBlendCmd ( "StackNoBlend", TRUE, _histBox, radList0 );
    _stackBlend = new SetStackBlendCmd ( "StackBlend", TRUE, _histBox, radList0 );
    _row = new SetPopUpDirRowCmd ( "Row", TRUE, _histBox, radList0 );
    _column = new SetPopUpDirColCmd ( "Column", TRUE, _histBox, radList0 );

    radList0->add( _stackNoBlend );
    radList0->add( _stackBlend );
    radList0->add( _row );
    radList0->add( _column );

    _stats = new StatsCmd ( "Show Stats", TRUE, _histBox );
    _axis = new AxisCmd ( "Show Axis", TRUE, _histBox );

    //  Create the Preferences Pulldown Menu objects as Radio Buttons
    //  Need to create 2 independent radio banks radList1 and radList2

    CmdList *radList1 = new CmdList();
    _vertical = new SetVertHistGraphCmd ( "Vertical HistGraph", TRUE, _histBox, radList1 );
    _horizontal = new SetHorHistGraphCmd ( "Horizontal HistGraph", TRUE, _histBox, radList1 );

    radList1->add( _vertical );
    radList1->add( _horizontal );

    CmdList *radList2 = new CmdList();
    _ascending = new SetAscAxisCmd ( "Ascending Axis Values", TRUE, _histBox, radList2 );    
    _descending = new SetDescAxisCmd ( "Descending Axis Values", TRUE, _histBox, radList2 );    

    radList2->add( _ascending );
    radList2->add( _descending );

    _vertical->addToActivationList (_ascending);
    _vertical->addToActivationList (_descending);

    _horizontal->addToDeactivationList (_ascending);
    _horizontal->addToDeactivationList (_descending);

    CustomDialog *dialog = new SpikeDialog (_histBox, "Spike");

   _spikeDialog = new SpikeDialogCmd( "Spike", TRUE, dialog );

   _logScale = new HistLogScaleCmd ("Log-Scaled Axis", TRUE, _histBox); 

   _histBox->manage();

   return (_form);
}

void HistWindow::createMenuPanes()
{
    MenuCmdList *cmdList;

    // Add Hist_View Pulldown Menu

    cmdList = new MenuCmdList("Hist_View");
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _stackNoBlend );
    cmdList->addRadioButton ( _stackBlend );   
    cmdList->addRadioButton ( _row );
    cmdList->addRadioButton ( _column );
    cmdList->addSeparator ();   
    cmdList->addCheckBox ( _stats );
    cmdList->addCheckBox ( _axis );
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _horizontal );
    cmdList->addRadioButton ( _vertical );
    cmdList->addSeparator ();   
    cmdList->addRadioButton ( _ascending );
    cmdList->addRadioButton ( _descending );
    cmdList->addSeparator ();   
    cmdList->addButton( _spikeDialog );
    cmdList->addSeparator ();
    cmdList->addCheckBox (_logScale );

    _menuBar->addCommands ( cmdList, FALSE );

    //  Add in PopupMenu for Button 3

    PopupMenu *popup = new PopupMenu(_form, "Hist Popup", cmdList);
    popup->attachPopup(_form);

    delete cmdList;
}
