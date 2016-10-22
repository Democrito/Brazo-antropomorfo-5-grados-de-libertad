# Anthropomorphic programmable robot arm 5 degrees of freedom (FreeBASIC + OpenGL)
[![License](http://img.shields.io/:license-gpl-blue.svg)](http://opensource.org/licenses/GPL-3.0)

![](https://sites.google.com/site/proyectosroboticos/_/rsrc/1293750569649/Descargar-Simuladores/Brazo%20robot%2010%20cubos.PNG)

"BrazoRobot.bas" and DiezCubos.bas "are the codes sources FreeBasic programming language of the two simulators robot arm that is on this website: https://sites.google.com/site/proyectosroboticos/Descargar-Simuladores

"RV-2AJ Emulator supports COSIMIR.bas" is the source code similar to the previous simulator, which also lets you read files ".POS" COSIMIR extension. More information: https://sites.google.com/site/proyectosroboticos/Descargar-Simuladores/simulador-rv-2aj

@.) Download all giving "Clone or download" (top right button green). You give the option to "Download ZIP" and downloaded. You open the zip and take the folder that is inside and put it on the desktop or wherever you more comfortable. You open the folder and there you have three codes sources .bas extension. Give that interests you and if you have installed the compiler you can compile and run.
The editor and compiler can download it here (size less than 9MB): http://tinyurl.com/jkpgyqx

@.) If the compile gives "Error 58" You have to remove the "windows.bi" file in that folder; I have put that file to be compatible with older versions of the compiler FreeBasic.

@.) Depending on the version of the compiler, functions "Multikey ()" can cause problems. If you get error in this role you should change all "FB.SC_" by "SC_", or vice versa (as appropriate), and matter resolved.

Example:

If Multikey(FB.SC_A) Then X = X+1

by

If Multikey(SC_A) Then X = X+1

So with all the "Multikey()" functions.

@.) The .plt files are examples for the robot arm draw its contents.

@.) The .xyz files are examples of pre-programmed movements; They are only compatible with "BrazoRobot.bas", the other two simulators will not know to read; if this is your case delete them.

@.) Complementary information: https://sites.google.com/site/proyectosroboticos/Descargar-Simuladores
