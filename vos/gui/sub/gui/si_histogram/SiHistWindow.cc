////////////////////////////////////////////////////////////////////
// SiHistWindow.cc:
////////////////////////////////////////////////////////////////////
#include "SiHistWindow.h"

#include "SiHistSetBlendCmd.h"
#include "SiHistSetStackCmd.h"
#include "SiHistSetRowCmd.h"
#include "SiHistSetColCmd.h"
#include "SiHistShowAxisCmd.h"
#include "SiHistShowStatCmd.h"
#include "SiHistSetAscAxisCmd.h"
#include "SiHistSetDescAxisCmd.h"
#include "SiHistSetHorGraphCmd.h"
#include "SiHistSetVerGraphCmd.h"
#include "SiHistSetLogScaleCmd.h"
#include "SiHistSpikeCmd.h"

#include "SiHistSpikeDialog.h"
#include "SpikeDialogCmd.h"
#include "MenuCmdList.h"
#include "SeparatorCmd.h"
#include "PopupMenu.h"
#include "SiHistogram.h"

#include "SiHistBox.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/RepType.h>

SiHistWindow::SiHistWindow ( const char *name, SiHistogram *histR, 
		SiHistogram *histG, SiHistogram *histB ) 
	: MenuWindow ( name )
{
    _histR = histR;
    _histG = histG;
    _histB = histB;

    _histBox = NULL;
    _popup = NULL;
}

SiHistWindow::~SiHistWindow()
{
    delete _histBox;
    delete _popup;
}

Widget SiHistWindow::createWorkArea ( Widget parent )
{
    _histBox = new SiHistBox ( parent, "histBox", _histR, _histG, _histB );

    // Create the Hist_VIEW Pulldown Menu objects
    
    CmdList *radList0 = new CmdList();
    _stackNoBlend = new SiHistSetStackCmd ( "StackNoBlend", TRUE, _histBox, radList0 );
    _stackBlend = new SiHistSetBlendCmd ( "StackBlend", TRUE, _histBox, radList0 );
    _row = new SiHistSetRowCmd ( "Row", TRUE, _histBox, radList0 );
    _column = new SiHistSetColCmd ( "Column", TRUE, _histBox, radList0 );

    radList0->add( _stackNoBlend );
    radList0->add( _stackBlend );
    radList0->add( _row );
    radList0->add( _column );

    _stats = new SiHistShowStatCmd ( "Show Stats", TRUE, _histBox );
    _axis = new SiHistShowAxisCmd ( "Show Axis", TRUE, _histBox );

    //  Create the Preferences Pulldown Menu objects as Radio Buttons
    //  Need to create 2 independent radio banks radList1 and radList2

    CmdList *radList1 = new CmdList();
    _vertical = new SiHistSetVerGraphCmd ( "Vertical HistGraph", TRUE, _histBox, radList1 );
    _horizontal = new SiHistSetHorGraphCmd ( "Horizontal HistGraph", TRUE, _histBox, radList1 );

    radList1->add( _vertical );
    radList1->add( _horizontal );

    CmdList *radList2 = new CmdList();
    _ascending = new SiHistSetAscAxisCmd ( "Ascending Axis Values", TRUE, _histBox, radList2 );    
    _descending = new SiHistSetDescAxisCmd ( "Descending Axis Values", TRUE, _histBox, radList2 );    

    radList2->add( _ascending );
    radList2->add( _descending );

    _vertical->addToActivationList (_ascending);
    _vertical->addToActivationList (_descending);

    _horizontal->addToDeactivationList (_ascending);
    _horizontal->addToDeactivationList (_descending);

    CustomDialog *dialog = new SiHistSpikeDialog (_histBox, "Spike");

   _spikeDialog = new SpikeDialogCmd( "Spike", TRUE, dialog );

   _logScale = new SiHistSetLogScaleCmd ("Log-Scaled Axis", TRUE, _histBox); 

    return (_histBox->baseWidget());
}

void SiHistWindow::createMenuPanes()
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

    _popup = new PopupMenu(_histBox->baseWidget(), "Hist Popup", cmdList);
    _popup->attachPopup(_histBox->baseWidget());

    delete cmdList;
}
