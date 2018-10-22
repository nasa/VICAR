rem  This is a sample DOS script to build a SPICE DLL.
rem  The correct path to your compiler, JDK, and SPICE Toolkit
rem  will have to be set below.
rem
set PATH="C:\Programs\DevStud\SharedIDE\BIN;C:\Programs\DevStud\VC\BIN";"C:\Programs\DevStud\VC\BIN\WIN95"
set INCLUDE=C:\Programs\DevStud\VC\INCLUDE
set LIB=C:\Programs\DevStud\VC\LIB
rem
echo Making jpl_mipl_spice.dll
rem
cl -Ic:\jdk13\include  -Ic:\jdk13\include\win32 -Ic:\programs\cspice\include  -LD jpl_mipl_spice_jni_SpiceLib.c -Fejpl_mipl_spice.dll /link c:\programs\cspice\lib\cspice.lib /NODEFAULTLIB:libcmt
