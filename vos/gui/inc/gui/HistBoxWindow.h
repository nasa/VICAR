////////////////////////////////////////////////////////////////
// HistBoxWindow.h:
////////////////////////////////////////////////////////////////
#ifndef HISTBOXWINDOW_H
#define HISTBOXWINDOW_H
#include "MainWindow.h"

class HistBox;

class HistBoxWindow : public MainWindow {

private:

   HistBox *_histBox;

protected:

   virtual Widget createWorkArea(Widget);

public:

   HistBoxWindow(char *);
   ~HistBoxWindow();
};

#endif

