$ set verify
$ set noon
$!
$ write sys$output "**********************************************************"
$ write sys$output "* Building MOTIFAPP applications                         *"
$ write sys$output "**********************************************************"
$!
$ if (f$getsyi("cpu").eq.128)
$ then                                  ! Alpha
$!
$! Did DEC even ever bloody TRY to compile X with C++??!!!
$!
$! Intrinsic.h (at least) has bugs where it does #ifdef VMS but not
$! #ifdef __VMS like it should to be compatible with ANSI C or C++
$! so we just define it on the command line.
$!
$! But, when we turn VMS on, the Xlib.h definition for Display breaks
$! because there's a "typedef _CBdata ... _XCBdata" and then a reference
$! to "struct _XCBdata ...", which is a typedef, not a structure name!
$! So, to get around this, redefine _XCBdata (which appears not to be used
$! otherwise) to be _CBdata to make the compiler happy.  Jeez.
$!
$ options="/float=d_float/define=(""VMS"",""_XCBdata=_CBdata"")/deb/noopt"
$!
$ cxx'options' BasicComponent.cc
$ cxx'options' UIComponent.cc
$ cxx'options' Application.cc
$ cxx'options' MainWindow.cc
$ cxx'options' Main.cc
$ cxx'options' DialogManager.cc
$ cxx'options' InfoDialogManager.cc
$ cxx'options' QuestionDialogManager.cc
$ cxx'options' AskFirstCmd.cc
$ cxx'options' Cmd.cc
$ cxx'options' CmdInterface.cc
$ cxx'options' CmdList.cc
$ cxx'options' NoUndoCmd.cc
$ cxx'options' UndoCmd.cc
$ cxx'options' WarnNoUndoCmd.cc
$ cxx'options' ButtonInterface.cc
$ cxx'options' MenuBar.cc
$ cxx'options' MenuWindow.cc
$ cxx'options' QuitCmd.cc
$ cxx'options' ManageCmd.cc
$ cxx'options' IconifyCmd.cc
$ cxx'options' WorkingDialogManager.cc
$ cxx'options' PixmapCycler.cc
$ cxx'options' BusyPixmap.cc
$ cxx'options' InterruptibleCmd.cc
$ cxx'options' SelectFileCmd.cc
$ cxx'options' ColorModel.cc
$ cxx'options' RGBController.cc
$ cxx'options' TextView.cc
$ cxx'options' SwatchView.cc
$ cxx'options' RGBView.cc
$ cxx'options' HSVView.cc
$ cxx'options' ColorChooser.cc
$ cxx'options' Clock.cc
$ cxx'options' ErrorDialogManager.cc
$ cxx'options' ListInterface.cc
$ cxx'options' NoOpCmd.cc
$ cxx'options' SeparatorInterface.cc
$ cxx'options' ToggleInterface.cc
$ cxx'options' CheckBoxInterface.cc
$ cxx'options' RadioButtonInterface.cc
$ cxx'options' RadioCmd.cc
$ cxx'options' CascadeInterface.cc
$ cxx'options' MenuItem.cc
$ cxx'options' MenuCmdList.cc
$ cxx'options' SeparatorCmd.cc
$ cxx'options' CustomDialog.cc
$ cxx'options' KeyinView.cc
$ cxx'options' RadioCmdBox.cc
$ cxx'options' OptionCmdMenu.cc
$ cxx'options' OptionInterface.cc
$ cxx'options' PopupMenu.cc
$ cxx'options' ArrowButtonInterface.cc
$ cxx'options' HelpBrowser.cc
$ cxx'options' HelpOnContextCmd.cc
$ cxx'options' HelpSelfCmd.cc
$ cxx'options' PrintWidgetTreeCmd.cc
$ cxx'options' FramedButtonInterface.cc
$ cxx'options' StringKeyinInterface.cc
$ cxx'options' MenuDialog.cc
$ cxx'options' PostSingleFileDialogCmd.cc
$ cxx'options' SingleFileSelBox.cc
$ cxx'options' SingleFileSelWindow.cc
$ cxx'options' KeyinViewBlanked.cc
$ cxx'options' PrefManager.cc
$ cxx'options' ErrorData.cc
$ cxx'options' ErrorManager.cc
$ cxx'options' LogWindow.cc
$ cxx'options' YesNoDialog.cc
$ cxx'options' CallbackCompressor.cc
$!
$ else					! VAX
$!  No C++, nothing to do
$ endif
$!
$!
$ if (f$getsyi("cpu").eq.128)
$ then					! Alpha
$   create/dir [.axp-vms]
$   library/create [.axp-vms]motifapp.olb *.obj
$ else					! VAX
$   write sys$output "C++ is not available on VAX/VMS (MotifApp library)"
$ endif
$!
$ set noverify
$!
$ exit
