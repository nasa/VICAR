#include "XvdApplication.h"
#include "ImageWindowBlink.h"
#include "BlinkControl.h"

Application * xvdApp = new XvdApplication("XVd_blink");
MainWindow * blinkControl = new BlinkControl("Blink");
// BlinkControl creates several ImageWindowBlink's.  Can't do it here because
// we want a loop.

