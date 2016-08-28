
WindowTitle "Brazo Robot RV-2AJ."

#Include Once "GL/gl.bi"
#Include Once "GL/glu.bi"
#Include Once "GL/glut.bi"
#Include Once "fbgfx.bi"                   
#Include Once "createtex.bi"
#Include Once "windows.bi"

Declare Sub InverseK
Declare Sub DibujaBrazo
Declare Sub Bresenham
Declare Sub Teclas
Declare Sub Scroll

Declare Sub FilesXYZ
Declare Sub Posicionate
Declare Sub PLT2BUFFER
Declare Sub File2Buffer
Declare Sub Buffer2File
Declare Sub FactorXY
Declare Sub CleanFile
Declare Sub HayCambios
Declare Sub Salvar
Declare Sub Ayuda

Declare Sub BuildFont
Declare Sub glPrint(ByVal As Integer, ByVal As Integer, ByVal As String, ByVal As Integer)

'-------- Declaración de variables ------------

Const Letras=6  ' Longitud de variables tipo string de cada grado de libertad. Para los ficheros.

Type Grado
     StrX As String*Letras
     StrY As String*Letras
     StrZ As String*Letras
     StrV As String*Letras
     StrW As String*Letras
     StrD As String*Letras
End Type

Dim Shared Eje  As Grado

Dim Shared As String*Letras Cadena

Dim Shared As Double   Pi, Rad, Grad, Pausa
Dim Shared As Double   AngGiro, AngBrazo, AngAntBr, AngMunecA, AngMunecB
Dim Shared As Single   LHombro, LBrazo, LAntBr, LMunec, LDedos, DistDedos, Espacio, EscenaY, EscenaX, EscenaZ
Dim Shared As Integer  LongBrazo, LongAntBr, LongMunec, LongDedos, AlturaH, Terminal

Dim Shared As Integer  EjeX, EjeY, EjeZ, EjeV, EjeW, EjeD
Dim Shared As Integer  Xold, Yold, Zold, Vold, Wold, Dold

Dim Shared As String   xx,   yy,   zz

Dim Shared As Single   Divisor
Dim Shared As UInteger StandBy, Ventana, NumFich, Result, Menu
   
Dim Shared As String   Fichero, OldFile, NomFich, Guiones, Tecla

Dim Shared As UInteger Modificado, Reg, Freg, Aux, Cont, Paso, Memo, F1, FN

Dim Shared As UInteger gbase
Dim Shared As UInteger texture

Dim   Shared As Integer Count, Flag, Neg
ReDim Shared As Single  Array(1 To 1, 1 To 1)

Dim Shared As Single LightPos(0 To 3) => {0.0, 5.0, -4.0, 1.0}     '' Light Position
Dim Shared As Single LightAmb(0 To 3) => {0.2, 0.2, 0.2, 1.0}      '' Ambient Light Values
Dim Shared As Single LightDif(0 To 3) => {0.8, 0.8, 0.8, 1.0}      '' Diffuse Light Values
Dim Shared As Single LightSpc(0 To 3) => {-0.2, -0.2, -0.2, 1.0}   '' Specular Light Values

Dim Shared quadratic As GLUquadricObj Ptr 

'------------Carga de Variables--------------------

Pi   = Atn(1) * 4
Rad  = Pi  / 180
Grad = 180 /  Pi

For Aux=1 To 6
   For Cont=1 To Letras
       Guiones=Guiones+"-"
   Next
   If Aux<6 Then Guiones=Guiones+" "
Next 

'-=- Ajustes del Brazo: Dimensiones, EscenaZs, etc. -=-

AlturaH    = 300     ' Altura del Hombro. (Distancia del suelo hasta la base del hombro.)
LongBrazo  = 250     ' Longitud Brazo.     
LongAntBr  = 160     ' Longitud AnteBrazo.
LongMunec  = 145     ' Longitud Muñeca.
LongDedos  =  50     ' Longitud Dedos.
Terminal   = LongMunec+LongDedos

EjeX=( 200)          ' Posicion Inicial X.  Aqui damos las coordenadas iniciales de la punta del brazo.
EjeY=(-100)          ' Posicion Inicial Y.  Se puede modificar los valores que están dentro del paréntesis.
EjeZ=( 250)          ' Procurar que esté dentro del área de trabajo.
EjeV=( 180)          ' Ang Relativo Inicial del cabeceo (Pich). 90 Grados es horizontal, por ejemplo.
EjeW=(   0)          ' Ang Relativo inicial del balanceo(Roll). 
EjeD=(   0)          ' Dedos cerrados para comenzar.

Xold=EjeX                   
Yold=EjeY
Zold=EjeZ
Vold=EjeV
Wold=EjeW
Dold=EjeD

'--- No tocar los valores de estas variables ---

LHombro    = AlturaH  /100
LBrazo     = LongBrazo/100   ' Equivalente en dimensiones de OpenGL.
LAntBr     = LongAntBr/100   ' No tocar aquí.
LMunec     = LongMunec/100
LDedos     = LongDedos/100
Espacio    = LBrazo+LAntBr

DistDedos  = .11             ' No tocar.

EscenaX=  0
EscenaY= 15
EscenaZ=-11

'---- Abro fichero para manejo del Scroll ------

Kill "Buffer.mem"
Open "Buffer.mem" For Random As (2) Len=Len(Grado)

'Screen 19, 16,, 2   ' Screen 19 es alta resolución 800x600
Screen 19, 1,, 2  
'----------------------------------
ReDim buffer(256*256*4+4) As UByte       
BLoad "Font.bmp", @buffer(0)            
texture = CreateTexture(@buffer(0))
BuildFont 
'----------------------------------

glViewport 0, 0, 800, 600
glMatrixMode GL_PROJECTION
glLoadIdentity
gluPerspective 45.0, 800.0/600.0, 0.1, 255.0
glMatrixMode GL_MODELVIEW
glLoadIdentity

glShadeModel GL_FLAT
glClearColor 0.0, 0.0, 0.0, 0.5
glClearDepth 1.0
glEnable GL_DEPTH_TEST
glDepthFunc GL_LEQUAL
glHint GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST

glBlendFunc GL_SRC_ALPHA, GL_ONE         
glEnable GL_TEXTURE_2D

'---------------------------- Luces ------------
glLightfv(GL_LIGHT1, GL_POSITION, @LightPos(0))       '' Set Light1 Position
glLightfv(GL_LIGHT1, GL_AMBIENT, @LightAmb(0))        '' Set Light1 Ambience
glLightfv(GL_LIGHT1, GL_DIFFUSE, @LightDif(0))        '' Set Light1 Diffuse
glLightfv(GL_LIGHT1, GL_SPECULAR, @LightSpc(0))       '' Set Light1 Specular
gldisable(GL_LIGHT1)                                   '' Enable Light1
gldisable(GL_LIGHTING)                                 '' Enable Lighting

quadratic = gluNewQuadric()                           '' Create A Pointer To The Quadric Object
gluQuadricNormals quadratic, GLU_SMOOTH               '' Create Smooth Normals
gluQuadricTexture quadratic, GL_TRUE                  '' Create Texture Coords


'-=-=-=- Programa Principal -=-=-=-

F1     =1
Memo   =F1
Reg    =1
Ventana=0
StandBy=0
Divisor=1

While (1)
   
      Tecla=InKey
      Teclas   
      
Wend

Sub Teclas
   
   If Tecla=Chr(255)+"<" Then  ' Si pulsas F2, cargar archivos POS y/o PLT, si los hubiera.
      Erase Array
      Flag=0
      Ventana=1
      Paso=0
      FN=5
      HayCambios
      
   EndIf
   
   If Tecla=Chr(255)+"=" Then  ' Si pulsas F3, guardar registro.
      Ventana=2
      Paso=0
   EndIf
   
   If Tecla=Chr(255)+";" Then F1 Xor = 1 ' Si pulsas F1, Quitar/Poner Ayuda en pantalla.
   
   If Ventana=0 And StandBy=0 Then
      
      If MultiKey(FB.SC_LSHIFT) Then 
            
         If LCase(Tecla)="a"   Then EjeX=EjeX-1
         If LCase(Tecla)="d"   Then EjeX=EjeX+1
         If LCase(Tecla)="s"   Then EjeY=EjeY-1
         If LCase(Tecla)="w"   Then EjeY=EjeY+1
         If LCase(Tecla)="e"   Then EjeZ=EjeZ-1
         If LCase(Tecla)="q"   Then EjeZ=EjeZ+1
         

         If LCase(Tecla)="x"   Then EjeV=EjeV-1
         If LCase(Tecla)="z"   Then EjeV=EjeV+1
   
         
         If LCase(Tecla)="v"   Then EjeW=EjeW-1       
         If LCase(Tecla)="c"   Then EjeW=EjeW+1 
            
      Else 
                  
         If MultiKey(FB.SC_A)  Then EjeX=EjeX-1
         If MultiKey(FB.SC_D)  Then EjeX=EjeX+1
         If MultiKey(FB.SC_S)  Then EjeY=EjeY-1
         If MultiKey(FB.SC_W)  Then EjeY=EjeY+1
         If MultiKey(FB.SC_E)  Then EjeZ=EjeZ-1
         If MultiKey(FB.SC_Q)  Then EjeZ=EjeZ+1
         

         If MultiKey(FB.SC_X)  Then EjeV=EjeV-1
         If MultiKey(FB.SC_Z)  Then EjeV=EjeV+1

         
         If MultiKey(FB.SC_V)  Then EjeW=EjeW-1
         If MultiKey(FB.SC_C)  Then EjeW=EjeW+1
            
         If LCase(Tecla)="m"   Then 
            If EjeD>0  Then    EjeD=EjeD-1
         EndIf    
         
         If LCase(Tecla)="n"   Then 
            If EjeD<25 Then    EjeD=EjeD+1
         EndIf
         
      EndIf

      If UCase(Right(Fichero,3))="POS" Or Fichero="" Then   ' Permite moverse sin límite por el registro si no es un PLT
         If (Tecla=Chr(255)+"H") And (Reg > 1)  Then  ' Decrementa contador registro.
             Reg=Reg-1
             Posicionate
         EndIf    
         If (Tecla=Chr(255)+"P")                Then  ' Incrementa contador registro.
             Reg=Reg+1
             Posicionate
         EndIf
      EndIf
      
       If UCase(Right(Fichero,3))="PLT" Then   ' Sólo cuando es un fichero PLT no podrá moverse más allá del último registro.
         If (Tecla=Chr(255)+"H") And (Reg > 1)  Then  ' Decrementa contador registro.
             Reg=Reg-1
             Posicionate
         EndIf    
         If (Tecla=Chr(255)+"P") And (Reg < ( Lof(2)/Len(grado)) )              Then  ' Incrementa contador registro.
             Reg=Reg+1
             Posicionate
         EndIf
      EndIf

      If Tecla=Chr(255)+"G" Then  ' Pulsando "Inicio", te lleva al valor del 1ºregistro.
         Reg=1   
         Posicionate
      EndIf
      
      If Tecla=Chr(255)+"O" Then  ' Pulsando "Fin", ir al último registro.
                  
         If UCase(Right(Fichero,3))<>"PLT" Or Fichero="" Then
            Reg=( Lof(2)/Len(Grado) )+1
         Else
            Reg=( Lof(2)/Len(Grado) )
         EndIf      	
      
         If Reg<1 Then Reg=1
         CleanFile
         Posicionate
      EndIf
      
      If ( Tecla=Chr(13) ) And ( UCase(Right(Fichero,3))<>"PLT" ) Then  ' Memoriza posición en el registro.
                                                                        ' excepto en los PLT.   
         Eje.StrX=Space(Letras)
         Eje.StrY=Space(Letras)
         Eje.StrZ=Space(Letras)
         Eje.StrV=Space(Letras)
         Eje.StrW=Space(Letras)
         Eje.StrD=Space(Letras)
         
         RSet Eje.StrX, Str(EjeX)
         RSet Eje.StrY, Str(EjeY)
         RSet Eje.StrZ, Str(EjeZ)
         RSet Eje.StrV, Str(EjeV)
         RSet Eje.StrW, Str(EjeW)
         RSet Eje.StrD, Str(EjeD)
         
         Put #2, Reg, Eje   
               
         Reg=Reg+1
         
         Posicionate
               
         Modificado=1
         
      EndIf
      
      If Tecla=Chr(255)+"R" Then ' Si pulsas Insertar, entonces desplazar siguientes.
         
         If Lof(2)>0 Then 
         
            Open "CleanFile.mem" For Random As (3) Len=Len(Grado)
            
            For Cont=1 To Reg-1
                 Get #2, Cont, Eje
                 Put #3, Cont, Eje 
            Next
            
            Eje.StrX=Space(Letras)
            Eje.StrY=Space(Letras)
            Eje.StrZ=Space(Letras)
            Eje.StrV=Space(Letras)
            Eje.StrW=Space(Letras)
            Eje.StrD=Space(Letras)
            
            Put #3, Reg, Eje

            For Cont=Reg To ( Lof(2)/Len(Grado) )
                 Get #2, Cont, Eje
                 Put #3, Cont+1, Eje 
            Next

            Close(2)   
            Kill "Buffer.mem"
            Open "Buffer.mem" For Random As (2) Len=Len(Grado)
            
            For Cont=1   To ( Lof(3)/Len(Grado) )
                  Get #3, Cont, Eje
                  Put #2, Cont, Eje
            Next
            
            Close(3)
            Kill "CleanFile.mem"
            
            Modificado=1
         
         EndIf    
         
      EndIf
      
      If Tecla=Chr(255)+"S" Then ' Si pulsas "Suprimir", entonces borras un registro.
      
         If Lof(2)>0 Then 
            
            Open "CleanFile.mem" For Random As (3) Len=Len(Grado)
            
            For Cont=1 To Reg-1
                 Get #2, Cont, Eje
                 Put #3, Cont, Eje 
            Next
            
            Aux=Lof(2)/Len(Grado)
            
            For Cont=Reg+1 To Aux
                 Get #2, Cont,   Eje
                 Put #3, Cont-1, Eje 
            Next
      
            Close(2)   
            Kill "Buffer.mem"
            Open "Buffer.mem" For Random As (2) Len=Len(Grado)
            
            For Cont=1   To ( Lof(3)/Len(Grado) )
                  Get #3, Cont, Eje
                  Put #2, Cont, Eje
            Next
            
            Close(3)
            Kill "CleanFile.mem"

            CleanFile
            Posicionate
            Modificado=1
            
         EndIf
         
      EndIf
      
      If Tecla=Chr(8) Then ' Si pulsas "BackSpace", entonces poner 'en blanco' el registro.
         
         Get #2, Reg, Eje
         
         If ( ( Eje.StrX<>Space(Letras) ) And ( Eje.StrX<>"" ) ) Then      
   
            RSet Eje.StrX,Space(Letras)
            RSet Eje.StrY,Space(Letras)
            RSet Eje.StrZ,Space(Letras)
            RSet Eje.StrV,Space(Letras)
            RSet Eje.StrW,Space(Letras)
            RSet Eje.StrD,Space(Letras)
            
            Put #2, Reg, Eje
            
            If Reg <> ( Lof(2)/Len(Grado) ) Then Reg=Reg+1
            
            Posicionate
            
            Modificado=1
            
         EndIf    
         
         CleanFile
         
      EndIf
      
      If (Tecla=Chr(255)+"?") Or (Tecla=Chr(255)+"@") Or FN=5 Then 'F5 ó F6
         
         Memo=F1
         If F1=1 Then F1=0
         
         If (Tecla=Chr(255)+"?") Then 
             Reg=1
             FN=5
         EndIf 
         If (Tecla=Chr(255)+"@") Then 
             FN=6
         EndIf 
         
         CleanFile
         Tecla=""
         WindowTitle "Ejecutando :  " + Fichero
         
         Aux=Lof(2)/Len(Grado)
         Freg=Reg
         For Cont=Freg To Aux
            
            Tecla=InKey 
            If Tecla=Chr(27) Then
                 Tecla=""
               Exit For
            EndIf 
            
            Reg=Cont
            Posicionate
            Bresenham
            
         Next
         Reg=Cont
         
         FN=0
         
         F1=Memo
         
         Get #2, 1, Eje
         
         If ( Eje.StrX=Space(Letras) Or Eje.StrX="" ) Then Reg=1
                     
         WindowTitle "Brazo Robot RV-2AJ."
         
      EndIf
      
      If Tecla=Chr(255)+">" Then ' F4, Registro nuevo
         Flag=0
         Menu=1
         Erase Array
         Divisor=1
         HayCambios
      EndIf
      
      If ( Tecla=Chr(255)+"k" ) Or ( Tecla=Chr(27) ) Then ' Salir del programa.
         Menu=2
         HayCambios
      EndIf    
   EndIf
   
   Bresenham

End Sub

Sub DibujaBrazo
   
   Dim As Single  ContX, ContY
   Dim As Integer Conta

'   ----------------------Animación OpenGL--------------------
   
   glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT)  
   glBindTexture GL_TEXTURE_2D, 0
   glLoadIdentity 
   glDisable GL_BLEND
   
   If Ventana=0 Then 
      If MultiKey (FB.SC_J) Then EscenaX=EscenaX-.5
      If MultiKey (FB.SC_L) Then EscenaX=EscenaX+.5
      If MultiKey (FB.SC_I) Then EscenaY=EscenaY+.5
      If MultiKey (FB.SC_K) Then EscenaY=EscenaY-.5
      If MultiKey (FB.SC_O) Then EscenaZ=EscenaZ+.1
      If MultiKey (FB.SC_U) Then EscenaZ=EscenaZ-.1
      
      If MultiKey (FB.SC_F7) Then 
         EscenaX= -0
         EscenaY= 15
         EscenaZ=-11
      EndIf
      
   EndIf

      
   '-------------- Plano Base ---------------

   glLoadIdentity
   glTranslatef       0.0, 0.0, EscenaZ              
   glRotatef EscenaX, 0.0, 1.0, 0.0
   glRotatef EscenaY, 1.0, 0.0, 0.0
   
   For    ContY=0 To Espacio Step .5
      For ContX=0 To Espacio Step .5
         glBegin(GL_LINE_LOOP)
            If F1=1 Then                                   
               glColor3f   0.1,  0.1,  0.2
            Else
               If Flag=1 Then                                   
                  glColor3f   0.1,   0.1,   0.2
               Else
                  glColor3f   1.0,   1.0,   1.0
               EndIf
            EndIf        
            glVertex3f  Contx, -LHombro-.01,  conty     
            glVertex3f -Contx, -LHombro-.01,  conty      
            glVertex3f -Contx, -LHombro-.01, -conty      
            glVertex3f  Contx, -LHombro-.01, -conty      
         glEnd
      Next
   Next
   
   If Flag = 1 Then 
      glBegin(GL_LINES)
         For Conta=2 To (Reg-1)
            If Array(Conta, 3)=0 Then
               glColor3f   0.0,  1.0,  1.0                             ' Crea el dibujo a base de líneas.
               glVertex3f  Array(Conta-1, 1), -LHombro, -Array(Conta-1, 2) 
               glVertex3f  Array(Conta,   1), -LHombro, -Array(Conta  , 2)
            EndIf     
         Next
      glEnd
   EndIf 
   
   glenable(GL_LIGHT1)
   glenable(GL_LIGHTING)

   '------ Hombro -------
   
   glTranslatef    0.0, 0.0, 0.0

   glRotatef AngGiro, 0.0, 1.0, 0.0

   glRotatef       90, 1.0, 0.0, 0.0 
   gluCylinder quadratic, 0.4, 0.4, LHombro, 18, 18
   glRotatef      -90, 1.0, 0.0, 0.0

   glutSolidSphere 0.5,   11,   11

   '--------- Brazo -----------
   glRotatef   AngBrazo, 0.0, 0.0, 1.0
   
   glTranslatef     0.0, 0.0,  0.5
   glutSolidSphere 0.3,   11, 11
   
   glRotatef       90, 0.0, 0.1, 0.0 
   gluCylinder quadratic, 0.2, 0.2, LBrazo, 18, 18
   glRotatef      -90, 0.0, 0.1, 0.0
   
   glTranslatef     0.0, 0.0, -1
   glutSolidSphere 0.3,   11, 11

   glRotatef       90, 0.0, 0.1, 0.0 
   gluCylinder quadratic, 0.2, 0.2, LBrazo, 18, 18
   glRotatef      -90, 0.0, 0.1, 0.0
   glTranslatef     0.0, 0.0,  0.5
   
   glTranslatef   LBrazo, 0.0, 0.5
   glutSolidSphere 0.3,   11,   11
   
   glTranslatef    0.0, 0.0,    -1.0
   glutSolidSphere 0.3,   11,   11

   ''------------------------Ant.Brazo-------------------
   '
   glRotatef   AngAntBr, 0.0, 0.0, 1.0
   glTranslatef    0.0,   0.0, 0.5
   glutSolidSphere 0.4,   11,   11
   
   glRotatef       90, 0.0, 0.1, 0.0 
   gluCylinder quadratic, 0.2, 0.2, LAntBr, 18, 18
   glRotatef      -90, 0.0, 0.1, 0.0
   
   glTranslatef    LAntBr,   0.0, 0.0
   glutSolidSphere 0.24,   11,   11
   
   ''------------------------ Muñeca ---------------------
   
   AngMunecB = EjeW
   
   glTranslatef            0.0, 0.0, -0.2
   glRotatef    AngMunecA, 0.0, 0.0, 1.0
   '----------------------------------------------
   glutSolidSphere 0.2,   11,   11

   glRotatef       90, 0.0, 0.1, 0.0 
   gluCylinder quadratic, 0.1, 0.1, LMunec-.6, 18, 18
   glRotatef      -90, 0.0, 0.1, 0.0
   '----------------------------------------------
   glTranslatef            0.0, 0.0, .4
   glutSolidSphere 0.2,   11,   11

   glRotatef       90, 0.0, 0.1, 0.0 
   gluCylinder quadratic, 0.1, 0.1, LMunec-.6, 18, 18
   glRotatef      -90, 0.0, 0.1, 0.0
   
   glTranslatef        Lmunec-.6, 0.0, -0.2
   glRotatef       90, 0.0, 0.1, 0.0 
   glutSolidTorus	(	 .1 , .2 , 10 , 10 )
   glRotatef      -90, 0.0, 0.1, 0.0
   
   glRotatef    AngMunecB, 1.0, 0.0, 0.0
   glutSolidSphere 0.2,   11,   11
   
   glRotatef       90, 0.0, 0.1, 0.0 
   gluCylinder quadratic, 0.15, 0.3, .6, 18, 18
   glRotatef      -90, 0.0, 0.1, 0.0

   '--------------Dedos------------------
   glBindTexture GL_TEXTURE_2D, 0   '********* Quitar textura **************
   
   If Flag=1 And EjeZ<51 Then
      
      glTranslatef    0.5, 0.0, 0
      glRotatef   90, 0.0, 1.0, 0.0
      glutSolidCone   0.1, 0.6, 8,  10
	      
   Else
	   
      DistDedos=(EjeD/100)+.075
      
      glTranslatef    0.60, 0.0, DistDedos
      
      glRotatef       94, 0.0, 1.0, 0.0
'      glRotatef       85, 0.0, 0.0, 1.0
      gluCylinder quadratic, 0.08, 0.04, LDedos, 6, 6
'      glRotatef      -85, 0.0, 0.0, 1.0
      glRotatef      -94, 0.0, 1.0, 0.0
      
      glTranslatef    0.0, 0.0, -2*DistDedos       ' En esta posición, 'DistDedos*(-2)' invierte
                                                   ' la posición 'DistDedos' de arriba. 
      glRotatef       84, 0.0, 1.0, 0.0
'      glRotatef      -85, 0.0, 0.0, 1.0
      gluCylinder quadratic, 0.08, 0.04, LDedos, 6, 6
'      glRotatef       85, 0.0, 0.0, 1.0
      glRotatef      -84, 0.0, 1.0, 0.0
   
   EndIf   
   
   glDisable(GL_LIGHT1)
   glDisable(GL_LIGHTING)
   
   '---- Marco del área de los ángulos ----
     
   If Ventana=0 Then
      glLoadIdentity
      glTranslatef  0.36,  0.25, -0.85
                 
      glBegin(GL_LINE_LOOP)                                                                 
   	  glColor3f    0.3,   0.00,   1.0               
   	  glVertex3f  -0.12,  -0.05,  0      
   	  glVertex3f   0.10,  -0.05,  0     
   	  glVertex3f   0.10,   0.05,  0  
   	  glVertex3f  -0.12,   0.05,  0 
   	glEnd 
   EndIf
   
   glEnable GL_BLEND 
   
   If Ventana=0 Then 
      glPrint 486, 420, "Ang. Giro Br.: " + Left  (Str( AngGiro),     8), 0
      glPrint 486, 410, "Ang. Brazo   : " + Left  (Str(-AngBrazo+90), 8), 0
      glPrint 486, 400, "Ang. Ant.Br. : " + Left  (Str(-AngAntBr),    8), 0
      glPrint 486, 390, "Ang. Muneca  : " + Left  (Str(-AngMunecA),   8), 0
   EndIf 
   
   Scroll
   
   If F1=1 Then Ayuda
   
   If Ventana=1 Or Ventana=2 Then FilesXYZ
   
   Flip    '<----- Muestra el gráfico por pantalla ------

'   Pausa=Timer+.0035     ' Temporizador para ordenadores rápidos.
'   While Pausa>Timer
'   Wend
     
End Sub


Sub FilesXYZ
   
   Static Fich(100) As String*25 
   Dim    As  Integer ContA, Aux1
   Static As UInteger Scr=1
   
   If Paso=0 Then
   	Paso   =1
   	NumFich=1
   	
   	If Ventana=2 Then
   	   Erase fich
   	EndIf    

   	For Aux1=1 To 2
   	   
   	   If   Aux1=1                  Then Fich(NumFich)=Dir("*.pos")
         If  (Aux1=2) And (Ventana=1) Then Fich(NumFich)=Dir("*.plt")
   	   
   	   If ( fich(1)="" ) And ( Ventana=1 ) And (Aux1=2) Then
   	   	MessageBox( NULL, "No existen ficheros *.pos para cargar.", "Brazo Robot RV-2AJ.", MB_ICONEXCLAMATION)
   	      Ventana=0
   	      Flag=0
   	   	Fichero=""
   	      Exit Sub
   	   EndIf    
   	
   	   While fich(NumFich)<>"" 
   	   	   NumFich=NumFich+1
   	         Fich(NumFich)=Dir()
   	   Wend
   	   
   	Next
   
   	NumFich=NumFich-1
   
   EndIf    
   
   Paso=1
   
   '-------------------------------------------------

   If (Tecla=Chr(255)+"H") And (Scr > 1)       And NomFich="" Then Scr=Scr-1
   If (Tecla=Chr(255)+"P") And (Scr < NumFich) And NomFich="" Then Scr=Scr+1
   
   If (Fich(Scr)="") And (Scr>1) Then Scr=Scr-1
   
   If Ventana=1 Then
      WindowTitle "Abrir Fichero de Posiciones."
      glColor3f 1,1,0
      glPrint 440,435, "Abrir:", 0
      NomFich=""
   EndIf
   
   If Ventana=2 Then 
      glColor3f 1,1,0
      glPrint 440,435, "Guardar:", 0
      If (Fich(1)="") And (Scr=1) Then
         glColor3f 1,0,0
         glPrint 505,380, "(Vacio)", 0
      EndIf
   EndIf  
   
   glBindTexture GL_TEXTURE_2D, 0
   glLoadIdentity 
   
   glDisable GL_BLEND 
     
   glTranslatef  0.23,  0.16, -0.63
                 
   glBegin(GL_LINE_LOOP)                                                                 
     glColor3f    0.3,   0.00,   1.0               
     glVertex3f  -0.1,  -0.05,  0      
     glVertex3f   0.1,  -0.05,  0     
     glVertex3f   0.1,   0.05,  0  
     glVertex3f  -0.1,   0.05,  0 
   glEnd
   	  
   If Ventana=2 Then
   	glBegin(GL_LINE_STRIP)                                                                 
   	  glColor3f    0.3,   0.0,   1.0   
   	            
   	  glVertex3f  -0.1,   0.00,   0      
   	  glVertex3f  -0.1,  -0.08,  0     
   	  glVertex3f   0.1,  -0.08,  0  
   	  glVertex3f   0.1,   0.00,   0 
   	glEnd
   EndIf 

   glEnable  GL_BLEND 

   For ContA=-2 To 2
   	If ContA=0 Then glColor3f 1, 1, 0 Else glColor3f 0.0, 0.0, 1.0
   	If Scr+ContA>0 Then   
   	   glPrint (440,380-(15*ContA),Fich(Scr+ContA),0)
   	EndIf 
   Next
   
   If Ventana=2 Then

   	If ((Tecla>Chr(45)) And (Tecla<Chr(123)) Or Tecla=Chr(8)) Then
         NomFich=NomFich+Tecla
         If (Tecla=Chr(8)) And (Len(NomFich)>0) Then 
            NomFich=Left(NomFich,Len(NomFich)-2)
         EndIf    
   	EndIf
   	
   	WindowTitle "Guardar Registro de Posiciones."
   	
   	glColor3f 1,1,0
   	glPrint 500,320, NomFich+".pos",0
   	
   	glColor3f 0,1,1
   	glPrint 450, 295, "Escribe un nombre, o bien,",0
   	glPrint 450, 280, "deja el nombre en blanco y",0
   	glPrint 450, 265, "elige en el 'scroll'.",0
   	
   EndIf    
   
   If Tecla=Chr(27) And Ventana=1 Then
   	Ventana=0
   	NomFich=""
   	Tecla=""
   	Exit Sub
   EndIf
   
   If Tecla=Chr(27) And Ventana=2 Then
      Ventana=0
      NomFich=""
      Tecla=""
      WindowTitle "Brazo Robot RV-2AJ."
      If Menu=2 Then End
      Exit Sub
   EndIf
   	
   If ( Tecla=Chr(13) ) And (Ventana=1) Then
   	
   	NomFich=""
   	Fichero=Fich(scr)
   	
   	If UCase(Right(Fichero,3))="PLT" Then  'Convierte y carga en Buffer el fichero PLT
         FactorXY
         PLT2BUFFER
         If (Reg <> 1) Then
            ReDim As Single  Array(1 To Reg-1, 1 To 3)
            For Count=1 To (Reg-1) 
                Get #2, Count, Eje
                Array(Count,1) = Val(Eje.StrX)/100
                Array(Count,2) = Val(Eje.StrY)/100
                Array(Count,3) = Val(Eje.StrZ)/100
            Next
            Flag = 1
         EndIf 
   	EndIf   	
   	If UCase(Right(Fichero,3))="POS" Then  'Carga el fichero POS en Buffer.
         File2Buffer
   	EndIf
   	
   	Nomfich=""
   	Paso=0
   	Ventana=0
   	Modificado=0
   	Reg=1
   	Posicionate

   EndIf
   
   If ( Tecla=Chr(13) ) And (NomFich<>"") And (Ventana=2) Then
      OldFile=Fichero
      Fichero=UCase(NomFich)+".POS"
      Salvar
      Flag=0
   EndIf 
   
   If ( Tecla=Chr(13) ) And (NomFich ="") And (Ventana=2) Then
      OldFile=Fichero
      Fichero=UCase(Fich(Scr))
      Salvar
      Flag=0
   EndIf

End Sub

Sub Scroll

   Dim As Integer       Cont2,  Vnum
   Dim As String*Letras Cadena, Vtext
      
   glColor3f 0.0,1.0,0.0 
      
   If (Fichero<>"") Then glPrint 370 ,460, "Nombre : "+Fichero, 0
      
   glPrint 280,  460, "Factor: " + Left( Str(Divisor), 5 ), 0
      
   RSet Cadena, Str(EjeX)
   glPrint 1, 460,                  Cadena+"X",     0
   RSet Cadena, Str(EjeY)
   glPrint 1+((Letras*6)+6),   460, Cadena+"Y",     0
   RSet Cadena, Str(EjeZ)
   glPrint 1+((Letras*12)+12), 460, Cadena+"Z",     0
   RSet Cadena, Str(EjeV)
   glPrint 1+((Letras*18)+18), 460, Cadena+"@"+"C", 0
   RSet Cadena, Str(EjeW)
   glPrint 1+((Letras*24)+24), 460, Cadena+"@"+"B", 0
   RSet Cadena, Str(EjeD)
   glPrint 1+((Letras*30)+30), 460, Cadena+"P",     0
      
   For Cont2=-2 To 2
      
      If Cont2=0 Then 
         glColor3f 1,1,1
         glPrint 255 ,420, "<---Reg:" + Str(Reg),0
      Else
         glColor3f 0,0,1
      EndIf    
         
      If (Cont2+Reg)>0 Then
            
         Get #2, (Reg+Cont2), Eje
            
         If ( ( Eje.StrX<>Space(Letras) ) And ( Eje.StrX<>"" ) ) Then
            glPrint 1,420-(12*Cont2),Eje.StrX+" "+Eje.StrY+" "+Eje.StrZ+" "+Eje.StrV+" "+Eje.StrW+" "+Eje.StrD,0
         Else
            glPrint 1,420-(12*Cont2), Guiones, 0
         EndIf
         
      EndIf      
      
   Next

End Sub

Sub InverseK
   
   Dim    As Double   Afx, Afy, LadoA, LadoB, Alfa, Beta, Gamma, Modulo, Hipotenusa, Xprima, Yprima
   Static As Integer  Xaux, Yaux, Zaux, Vaux, VEje
   Static As Integer  x, y, z, v
   
'     ------- Cinemática Inversa ----------

   Modulo  = Sqr(Abs(EjeX^2)+Abs(EjeY^2))
   AngGiro = (ATan2(EjeY, EjeX))*Grad
   
   Xprima=modulo
   Yprima=EjeZ
   
'   En esta subrutina, sustituimos EjeV por Veje (sólo afecta aquí), para hacer una modificación de grados. 
   VEje=-EjeV+90 ' Compativilidad de ángulos con COSIMIR.
   
   Afx=Cos(Rad*VEje)*Terminal
   LadoB=Xprima-Afx
   
   Afy=Sin(Rad*VEje)*Terminal
   LadoA=Yprima-Afy-AlturaH
    
   Hipotenusa=Sqr((LadoA^2)+(LadoB^2))
   
   Alfa=ATan2(LadoA,LadoB)
   
   Beta=Acos(((LongBrazo^2)-(LongAntBr^2)+(Hipotenusa^2))/((2*LongBrazo)*Hipotenusa))
   
   AngBrazo= (Alfa+Beta)*Grad          ' Ang. BRAZO     (en Grados).
   
   Gamma=Acos(((LongBrazo^2)+(LongAntBr^2)-(Hipotenusa^2))/((2*LongBrazo)*LongAntBr))
   AngAntBr=(-((180*Rad)-Gamma))*Grad  ' Ang. ANTEBRAZO (en Grados).
   AngMunecA= (Veje-AngBrazo-AngAntBr) ' Ang. Cabeceo   (en Grados).
   
   If (Str(AngBrazo)="-1.#IND") Or (Str(AngAntBr)= "-1.#IND") Or  (Str(AngMunecA)= "-1.#IND") Then 
      
      EjeX=Xaux       'Si hay ángulos imposibles, pasar a posición anterior.
      EjeY=Yaux
      EjeZ=Zaux
      EjeV=Vaux
      
      InverseK
      
   EndIf
      
   Xaux=EjeX
   Yaux=EjeY
   Zaux=EjeZ
   Vaux=EjeV
      
   DibujaBrazo
         
End Sub

Sub BuildFont                            
   
   Dim    gloop As Integer 
   Static cx    As Single                 
   Static cy    As Single                 

   gbase = glGenLists(256)                
   glBindTexture GL_TEXTURE_2D, texture     
   For gloop = 0 To 255             

      cx = (gloop Mod 16)/16         
      cy = (gloop\16)/16              

      glNewList gbase+gloop, GL_COMPILE  
      glBegin GL_QUADS                    
         glTexCoord2f cx, 1-cy-0.0625
         glVertex2i 0, 0                    
         glTexCoord2f cx+0.0625, 1-cy-0.0625
         glVertex2i 12,0      
         glTexCoord2f cx+0.0625, 1-cy  
         glVertex2i 12, 12           
         glTexCoord2f cx,1-cy       
         glVertex2i 0, 12            
      glEnd                            
      glTranslated 6, 0, 0                
      glEndList                     
   Next                                      
End Sub

Sub glPrint(ByVal x As Integer, ByVal y As Integer, ByVal glstring As String, ByVal gset As Integer)

   If gset>1 Then gset=1

   glBindTexture GL_TEXTURE_2D, texture             
   glDisable GL_DEPTH_TEST                                
   glMatrixMode GL_PROJECTION                              
   glPushMatrix                                          
      glLoadIdentity                                      
      glOrtho 0, 640, 0, 480,-1, 1                    
      glMatrixMode GL_MODELVIEW                          
      glPushMatrix                                       
         glLoadIdentity                                       
         glTranslated x, y, 0                              
         glListBase gbase-32+(128*gset)                     
         glCallLists Len(glstring),GL_BYTE, StrPtr(glstring) 
         glMatrixMode GL_PROJECTION                          
      glPopMatrix                                        
      glMatrixMode GL_MODELVIEW                         
   glPopMatrix                                          
   glEnable GL_DEPTH_TEST                           
End Sub

Sub PLT2BUFFER

   Dim    As String*1 Char
   Dim    As String   Cadena
   Dim    As Integer  CordX, CordY, CordZ
   Dim    As Integer  Bandera
   Dim    As String   Xr, Yr, Zr
   
   Close(2)   
   Kill "Buffer.mem"
   Open "Buffer.mem" For Random As (2) Len=Len(Grado)   

   Open Fichero For Binary As (1)
   
   Reg=1
   
   Do 
      Bandera = 0
      Get #1,, Char
          
      If Char="P" Then
                  
         Get #1,, Char
             
         If Char="R" Then
         	MessageBox( NULL, "El archivo: " + UCase(Fichero) + Chr(13)+ "Es incompatible con este programa porque contiene posiciones relativas.", "Brazo Robot.", MB_ICONEXCLAMATION)
                
         	Close(2)   
            Kill "Buffer.mem"
            Open "Buffer.mem" For Random As (2) Len=Len(Grado)
            Fichero=""
            Erase Array
            
            Flag=0    
            Reg=1
            Divisor=1
                  
            EjeX=Xold
         	EjeY=Yold
      	   EjeZ=Zold   
              
         	Exit Do   ' El fichero #1 se cerrará al salir del Do..Until.
         EndIf
          
         If Char="D" Or Char="A" Or Char="U" Then
             
            Bandera = 1
                 
            If Char="U" Then
               CordZ=20
               zz=Space(Letras-2)+"20"
            EndIf
              
            If Char="D" Then 
               CordZ=0
               zz=Space(Letras-1)+"0"
            EndIf
                       
            Cadena = ""
            Get #1,, Char
              
            If (Char > Chr(47)) And (Char < Chr(58)) Or Char="-" Then 
                        
                Do   
                     Cadena+= Char
                     Get #1,, Char
                Loop Until Char="," Or Eof(1)
                
                CordX=Val(Cadena)*(Divisor*1.5)
                xx=Str(CordX)
                Cadena = ""
                  
                Get #1,, Char
                      
                Do
                     Cadena+= Char
                     Get #1,, Char 
                Loop Until (Char < Chr(48)) Or (Char > Chr(57))  And Char<>"-"
                         
                CordY=Val(Cadena)*(Divisor*1.5)
	             If Neg=0 Then CordY=CordY-250
	             yy=Str(CordY)
	             Cadena = ""  
                      
            EndIf 
              
         EndIf
          
      EndIf
      
      If Zr<>zz And Reg>1 Then
      	
      	Get #2, Reg-1, Eje

		   RSet Eje.StrZ, Str(zz)
		
			Put #2, Reg, Eje
			
         Reg=Reg+1
         
         Zr=zz
      	
      EndIf

      If (Xr<>xx) Or (Yr<>yy) Then  ' Xr Yr evitan redundancia.                                                 ' Bandera evita la posición cero.
         Eje.StrX=Space(Letras)
		   Eje.StrY=Space(Letras)
		   Eje.StrZ=Space(Letras)
		   
		   RSet Eje.StrX, Str(xx)
		   RSet Eje.StrY, Str(yy)
		   RSet Eje.StrZ, Str(zz)
		   
		   Eje.StrV=Space(Letras-3)+"180"
		   Eje.StrW=Space(Letras-1)+  "0"
		   Eje.StrD=Space(Letras-1)+  "0"
		
		   Put #2, Reg, Eje
			
         Reg=Reg+1
         Xr=xx
         Yr=yy
         Zr=zz
      EndIf
         
   Loop Until Eof(1)
       
   Close(1)
       
End Sub

Sub File2Buffer
       
   Dim Char   As String*1
   Dim char2  As String*1
   Dim Cadena As String
   
   Dim Cont   As Integer
   Dim Aux    As Integer
   
   Dim        As Boolean   Flag
   
   Dim        As String    Xin, Yin, Zin, Vin, Win, Din


   Close
   Kill "Buffer.mem"
   Open "Buffer.mem" For Random  As (2) Len=Len(Grado)
   Open   Fichero    For Binary  As (3)
   
   Cont = 1
   Flag = FALSE
   
   While Not EOF(3)
   
   	Do
   	   Get #3,, Char
   	   
   	   If Char = "P" Then
   	   	Get #3,, Char
   	   	If Char = "D" Then
   	   	   Get #3,, Char
   	   	   If Char = " " Then
   	   	      Flag = TRUE
   	   	   Else
   	   	      Flag = FALSE
   	   	   EndIf
   	   	EndIf   
   	   EndIf 
   	   
   	   If Char = "="     Then Cadena = ""
   	   If Char = " "     Then Cadena = ""
   	   
   	   If Char = "*" Then
   	   	Get #3,, Char
   	   	If Char = "/" Then
   	   	   Get #3,, Char
   	   	   If Char = "/" Then
   	   	      Do   
   	   	         Get #3,, Char
   	   	      Loop Until Char = Chr(13) Or EOF(3)
   	   	   EndIf
   	   	EndIf
   	   EndIf
   	   
   	   If Char = Chr(13) Then
   	   	
   	   	Eje.StrX=Space(Letras)
   	   	Eje.StrY=Space(Letras)
   	   	Eje.StrZ=Space(Letras)
   	   	Eje.StrV=Space(Letras)
   	   	Eje.StrW=Space(Letras)
   	   	Eje.StrD=Space(Letras)
   	   	   	   	
   	   	RSet Eje.StrX=Xin
   	   	RSet Eje.StrY=Yin
   	   	RSet Eje.StrZ=Zin
   	   	RSet Eje.StrV=Vin
   	   	RSet Eje.StrW=Win
   	   	RSet Eje.StrD="     0"
   	      
   	      Put #2,, Eje
   	      Cont=1
   	      Cadena=""
   	   	
   	   EndIf
   	
   	Loop Until (Char > "+") And (Char < ":") And (Char <> "/") Or (Char = ")") Or EOF(3) Or char="P"
   	
   	If Char = "," Or Char = ")" Or char="P" Then
   	   If Cadena <> ""  Then
   	   	If Flag Then
   	   	   If Cont=2 Then Xin = Cadena
   	   	   If Cont=3 Then Yin = Cadena
   	   	   If Cont=4 Then Zin = Cadena
   	   	   If Cont=5 Then Win = Cadena
   	   	   If Cont=6 Then Vin = Cadena
   	   	Else
   	   	   If Cont=1 Then Xin = Cadena
   	   	   If Cont=2 Then Yin = Cadena
   	   	   If Cont=3 Then Zin = Cadena
   	   	   If Cont=4 Then Win = Cadena
   	   	   If Cont=5 Then Vin = Cadena
   	   	EndIf   
   	   	Cadena = ""
   	   	Cont+=1
   	   EndIf
   	Else
   	   Cadena += Char
   	EndIf
 
   Wend

   Close(3)
   Close(2)   
   	
   Open "Buffer.mem" For Random As (2) Len=Len(Grado)

End Sub


Sub Buffer2File
       
   Dim As UInteger Cont, Aux
   
   Kill   Fichero
   Open   Fichero    For Binary  As (3)
   
   Aux=Lof(2)/Len(Grado)
   
   For Cont=1 To Aux
   	Get #2, Cont, Eje
   	Put #3,, "Pos"+Str(Cont)+"=("
   	Put #3,, Str(Val(Eje.StrX))
   	Put #3,, ","
   	Put #3,, Str(Val(Eje.StrY))
   	Put #3,, ","
   	Put #3,, Str(Val(Eje.StrZ))
   	Put #3,, ","
   	Put #3,, Str(Val(Eje.StrW))
   	Put #3,, ","
   	Put #3,, Str(Val(Eje.StrV))
   	Put #3,, ",0.00)(6,0)"
   	Put #3,, Chr(13,10)
   Next 
   
   Close(3)

End Sub

Sub Posicionate
    
   Get #2, Reg, Eje
            
   If ( ( Eje.StrX<>Space(Letras) ) And ( Eje.StrX<>"" ) ) Then
      EjeX=Val(Eje.StrX)
      EjeY=Val(Eje.StrY)
      EjeZ=Val(Eje.StrZ)
      EjeV=Val(Eje.StrV)
      EjeW=Val(Eje.StrW)
      EjeD=Val(Eje.StrD)
   EndIf 

End Sub

Sub FactorXY

   Dim  As String*1 Char
   Dim  As String   Aux
   Dim  As Integer  CooX, CooY, Xmax, Ymax, XYmax

   Xmax=-65000
   Ymax=-65000
   
   Open Fichero For Binary Access Read As (1)
   
   While Not EOF(1)
   
      Get #1,, Char
      
      If Char="P" Then
         
         Get #1,, Char
         
         Select Case  Char
            
            Case "U"            
                 'GoTo Argumento
                 
            Case "D" 
                 GoTo Argumento
         
            Case "A"
                 GoTo Argumento
                 
         End Select
      
      EndIf
      
      GoTo salida
      
      Argumento:
      
         Get #1,, Char
         
         If (Char < Chr(45)) Or (Char > Chr(57)) Then GoTo salida
         
         Aux=""
         While (Char > Chr(44)) And (Char < Chr(58))
            Aux=Aux+Char
            Get #1,, Char
         Wend
        
         If Len(Aux)>0 Then
            CooX=Val(Aux)
            If CooX>Xmax Then Xmax=CooX
         EndIf    
         
         While (Char < Chr(45)) Or (Char > Chr(57))
            Get #1,, Char
         Wend 
         
         Aux=""
         While (Char > Chr(44)) And (Char < Chr(58))
            Aux=Aux+Char
            Get #1,, Char
         Wend
         
         If Len(Aux)>0 Then
            CooY=Val(Aux)
            If CooY>Ymax Then Ymax=CooY            
         EndIf
         
      salida:
         
   Wend
   
   Close (1)
   
   If Xmax>Ymax Then
      XYmax=Xmax
   Else
      XYMax=Ymax
   EndIf
   
   Divisor=(Espacio*50)/XYmax

End Sub

Sub CleanFile
       
   Dim As UInteger Cont, Cont1
   
   Open "CleanFile.mem" For Random  As (4) Len=Len(Grado)
   
   Cont=Lof(2)/Len(Grado)
   
   While (Cont > 1)
      
      Get #2, Cont, Eje
      
      If ( Eje.StrX<>"" ) And ( Eje.StrX<>Space(Letras) ) Then
         Exit While 
      EndIf
      
      Cont=Cont-1
      
   Wend 
   
   For Cont1=1 To Cont
       Get #2, Cont1, Eje
       Put #4, Cont1, Eje
   Next 
   
   Close(2)   
   Kill "Buffer.mem"
   Open "Buffer.mem" For Random As (2) Len=Len(Grado)
   
   For Cont1=1 To ( Lof(4)/Len(Grado) )
       Get #4, Cont1, Eje
       Put #2, Cont1, Eje
   Next
   
   Close(4)
   Kill "CleanFile.mem"
   
End Sub

Sub HayCambios
   
   If Modificado=1 Then
      Result = 0
      If Menu<>2 Then Result = MessageBox(0, "Quieres guardar los cambios?","BrazoRobot",MB_YESNO)
      If Menu =2 Then Result = MessageBox(0, "Quieres guardar los cambios antes de Salir?","BrazoRobot",MB_YESNO)
      If Result = IDYES Then
         Tecla=""
         Fichero=""
         Ventana=2
      Else
         If Menu=1 Then
            Menu=0
            Fichero=""
            Close(2)
            Kill "Buffer.mem"
            Open "Buffer.mem" For Random As #2
            Modificado=0
            Reg=1
         EndIf
         If Menu=2 Then
            End 
         EndIf
      EndIf
   Else
      If Menu=1 Then
         Menu=0
         Fichero=""
         Close(2)
         Kill "Buffer.mem"
         Open "Buffer.mem" For Random As #2
         Modificado=0
         Reg=1
      EndIf
      If Menu=2 Then
         End 
      EndIf
   EndIf
   
End Sub

Sub Salvar
   
   
   If Open( Fichero For Random Access Read As #3)=0 Then
         Close(3)
         Result = 0
         Result = MessageBox(0, "Quieres reemplazar el archivo existente?","BrazoRobot",MB_YESNO)
         If Result=IDYES Then
            Kill Fichero
            Buffer2File
         Else
            Fichero=OldFile
         EndIf    
      Else
         Buffer2File
      EndIf
      Paso=0
      NomFich=""
      Ventana=0
      Divisor=1
      If Menu=0 Then Modificado=0
      If Menu=1 Then
         If Fichero<>"" Then 
            Menu=0
            Fichero=""
            Close(2)
            Kill "Buffer.mem"
            Open "Buffer.mem" For Random As #2
            Modificado=0
            Reg=1
         EndIf 
      EndIf
      If Menu=2 And Tecla=Chr(13) Then
         End
      EndIf
   
End Sub

Sub Ayuda
   
   glColor3f 1.0, 0.0, 0.0
   glPrint 400, 240, "F1 -> Quitar/Poner esta Ayuda." , 0
   glPrint 1, 370, "Teclas:" , 0
   
   glColor3f .7,.7,.7
   glPrint 400, 230, "F2 Cargar Registro de Posiciones." , 0
   glPrint 400, 220, "F3 Salvar Registro de Posiciones." , 0
   glPrint 400, 210, "F4 Crear nuevo Registro de Posiciones." , 0
   glPrint 400, 200, "F5 Ejecutar desde el comienzo." , 0
   glPrint 400, 190, "F6 Ejecutar desde posicion actual." , 0
   glPrint 400, 180, "F7 Restaurar valores del Escenario." , 0
   
   glPrint 1, 350, "A D -> Mueve el terminal sobre el Eje X." , 0
   glPrint 1, 340, "W S -> Mueve el terminal sobre el Eje Y." , 0
   glPrint 1, 330, "Q E -> Mueve el terminal sobre el Eje Z." , 0
   glPrint 1, 320, "Z X -> Angulo Cabeceo." , 0
   glPrint 1, 310, "C V -> Angulo Balanceo." , 0
   glPrint 1, 300, "N M -> Abre/Cierra Mano." , 0
   glPrint 1, 280, "Shift + ADWSQEZXCV -> Mover con Precision." , 0
   
   glColor3f .4,.4,.4
   glPrint 1, 260, "J L -> Mueve Escenario sobre el Eje X." , 0
   glPrint 1, 250, "I K -> Mueve Escenario sobre el Eje Y." , 0
   glPrint 1, 240, "U O -> Aleja/Acerca Escenario." , 0
   glPrint 1, 220, "Pulsa 'Alt y 'Enter' para pantalla completa." , 0

   glColor3f .9,.3,.3
   glPrint 130, 70, "'Fechas' Arriba/Abajo para moverte dentro del Registro y en el Menu." , 0
   glPrint 130, 60, "'Enter' memoriza posicion en el Registro.", 0
   glPrint 130, 50, "'Inicio' ir al inicio del Registro de posiciones." , 0
   glPrint 130, 40, "'Fin' ir al final del Registro de posiciones." , 0
   glPrint 130, 30, "'BackSpace' borra las posiciones XYZ de un Registro.", 0 
   glPrint 130, 20, "'Suprimir' elimina una posicion en el Registro." , 0
   glPrint 130, 10, "'Insertar' inserta una posicion en el Registro." , 0
   
End Sub

Sub Bresenham

Dim As Integer   d1,   d2,   d3,   d4,   d5,   d6,_
                Ad1,  Ad2,  Ad3,  Ad4,  Ad5,  Ad6,_
               inc1, inc2, inc3, inc4, inc5, inc6,_
               d1x2, d2x2, d3x2, d4x2, d5x2, d6x2,_
               dim1, dim2, dim3, dim4, dim5, dim6,_
               err1, err2, err3, err4, err5, Conta                  
   StandBy=1

   dim1=Xold
   dim2=Yold
   dim3=Zold
   dim4=Vold
   dim5=Wold
   dim6=Dold

   d1 = EjeX - Xold
   d2 = EjeY - Yold
   d3 = EjeZ - Zold
   d4 = EjeV - Vold
   d5 = EjeW - Wold
   d6 = EjeD - Dold
   
   If (d1 < 0) Then 
   	 inc1 = -1
   Else
   	 inc1 =  1
   EndIf
   
   If (d2 < 0) Then 
     inc2 = -1
   Else 
     inc2 =  1
   EndIf 
   	
   If (d3 < 0) Then 
   	inc3 = -1
   Else
   	inc3 =  1
   EndIf
   
   If (d4 < 0) Then 
   	inc4 = -1
   Else
   	inc4 =  1
   EndIf

   If (d5 < 0) Then 
   	inc5 = -1
   Else
   	inc5 =  1
   EndIf
   
   If (d6 < 0) Then 
   	inc6 = -1
   Else
   	inc6 =  1
   EndIf
   
   Ad1 = Abs(d1)
   Ad2 = Abs(d2)
   Ad3 = Abs(d3)
   Ad4 = Abs(d4)
   Ad5 = Abs(d5)
   Ad6 = Abs(d6)
   
   d1x2 = Ad1*2
   d2x2 = Ad2*2
   d3x2 = Ad3*2
   d4x2 = Ad4*2
   d5x2 = Ad5*2
   d6x2 = Ad6*2
   
   If (Ad1>= Ad2) And (Ad1>= Ad3) And (Ad1>= Ad4) And (Ad1>= Ad5) And (Ad1>= Ad6) Then 
   	
   	err1 = d2x2 - Ad1
   	err2 = d3x2 - Ad1
   	err3 = d4x2 - Ad1
   	err4 = d5x2 - Ad1
   	err5 = d6x2 - Ad1
   	
   	For Conta = 1 To Ad1
   	   
   	   If (err1 > 0) Then
   	       dim2+= inc2
   	       err1 -= d1x2
   	   EndIf 
   	   
   	   If (err2 > 0) Then 
   	       dim3+= inc3
   	       err2 -= d1x2
   	   EndIf 
   	   
   	   If (err3 > 0) Then 
   	       dim4+= inc4
   	       err3 -= d1x2
   	   EndIf 

   	   If (err4 > 0) Then 
   	       dim5+= inc5
   	       err4 -= d1x2
   	   EndIf 
   	   
   	   If (err5 > 0) Then 
   	       dim6+= inc6
   	       err5 -= d1x2
   	   EndIf 
   	   
   	   err1 += d2x2
   	   err2 += d3x2
   	   err3 += d4x2
   	   err4 += d5x2
   	   err5 += d6x2
   	   
   	   dim1+= inc1
   	 
   	   EjeX=dim1
   	   EjeY=dim2
   	   EjeZ=dim3
   	   EjeV=dim4
   	   EjeW=dim5
   	   EjeD=dim6
   	   
   	   InverseK

   	Next
   	
   EndIf
   
   If (Ad2> Ad1) And (Ad2>= Ad3) And (Ad2>= Ad4) And (Ad2>= Ad5) And (Ad2>= Ad6)  Then 
   	
   	err1 = d1x2 - Ad2
   	err2 = d3x2 - Ad2
   	err3 = d4x2 - Ad2
   	err4 = d5x2 - Ad2
   	err5 = d6x2 - Ad2
   	
   	For Conta = 1 To  Ad2
   	   
   	   If (err1 > 0) Then 
   	       dim1+= inc1
   	       err1 -= d2x2
   	   EndIf 
   	   
   	   If (err2 > 0) Then 
   	       dim3+= inc3
   	       err2 -= d2x2
   	   EndIf 
   	   
   	   If (err3 > 0) Then 
   	   	 dim4+= inc4
   	   	 err3 -= d2x2
   	   EndIf
   	   
   	   If (err4 > 0) Then 
   	       dim5+= inc5
   	       err4 -= d2x2
   	   EndIf 
   	   
   	   If (err5 > 0) Then 
   	       dim6+= inc6
   	       err5 -= d2x2
   	   EndIf 
   	    
   	   err1 += d1x2
   	   err2 += d3x2
   	   err3 += d4x2
   	   err4 += d5x2
   	   err5 += d6x2
   	   
   	   dim2+= inc2
   	   
   	   EjeX=dim1
   	   EjeY=dim2
   	   EjeZ=dim3
   	   EjeV=dim4
   	   EjeW=dim5
   	   EjeD=dim6
   	   
   	   InverseK
   	   
   	Next
   	
   EndIf
   
   If (Ad3> Ad1) And (Ad3> Ad2) And (Ad3>= Ad4) And (Ad3>= Ad5) And (Ad3>= Ad6) Then 
   	
   	err1 = d1x2 - Ad3
   	err2 = d2x2 - Ad3
   	err3 = d4x2 - Ad3
   	err4 = d5x2 - Ad3
   	err5 = d6x2 - Ad3
   	
   	For Conta = 1 To Ad3
   	   
   	   If (err1 > 0) Then
   	       dim1+= inc1
   	       err1 -= d3x2
   	   EndIf 
   	   
   	   If (err2 > 0) Then 
   	       dim2+= inc2
   	       err2 -= d3x2
   	   EndIf 
   	   
   	   If (err3 > 0) Then 
   	       dim4+= inc4
   	       err3 -= d3x2
   	   EndIf 
   	   
   	   If (err4 > 0) Then 
   	       dim5+= inc5
   	       err4 -= d3x2
   	   EndIf 
   	   
   	   If (err5 > 0) Then 
   	       dim6+= inc6
   	       err5 -= d3x2
   	   EndIf 
   	   
   	   err1 += d1x2
   	   err2 += d2x2
   	   err3 += d4x2
   	   err4 += d5x2
   	   err5 += d6x2
   	   
   	   dim3+= inc3

   	   EjeX=dim1
   	   EjeY=dim2
   	   EjeZ=dim3
   	   EjeV=dim4
   	   EjeW=dim5
   	   EjeD=dim6
   	   
   	   InverseK

   	Next
   	
   EndIf
   
   If (Ad4> Ad1) And (Ad4> Ad2) And (Ad4> Ad3) And (Ad4>= Ad5) And (Ad4>= Ad6) Then 
   	
   	err1 = d1x2 - Ad4
   	err2 = d2x2 - Ad4
   	err3 = d3x2 - Ad4
   	err4 = d5x2 - Ad4
   	err5 = d6x2 - Ad4
   	
   	For Conta = 1 To Ad4
   	   
   	   If (err1 > 0) Then
   	       dim1+= inc1
   	       err1 -= d4x2
   	   EndIf 
   	   
   	   If (err2 > 0) Then 
   	       dim2+= inc2
   	       err2 -= d4x2
   	   EndIf 
   	   
   	   If (err3 > 0) Then 
   	       dim3+= inc3
   	       err3 -= d4x2
   	   EndIf 
   	   
   	   If (err4 > 0) Then 
   	       dim5+= inc5
   	       err4 -= d4x2
   	   EndIf 
   	   
   	   If (err5 > 0) Then 
   	       dim6+= inc6
   	       err5 -= d4x2
   	   EndIf 
   	   
   	   err1 += d1x2
   	   err2 += d2x2
   	   err3 += d3x2
   	   err4 += d5x2
   	   err5 += d6x2
   	   
   	   dim4+= inc4
   	   	   
   	   EjeX=dim1
   	   EjeY=dim2
   	   EjeZ=dim3
   	   EjeV=dim4
   	   EjeW=dim5
   	   EjeD=dim6
   	   
   	   InverseK
   	   
   	Next
   	
   EndIf
   
   If (Ad5> Ad1) And (Ad5> Ad2) And (Ad5> Ad3) And (Ad5> Ad4) And (Ad5>= Ad6) Then 
   
   	err1 = d1x2 - Ad5
   	err2 = d2x2 - Ad5
   	err3 = d3x2 - Ad5
   	err4 = d4x2 - Ad5
   	err5 = d6x2 - Ad5
   	
   	For Conta = 1 To Ad5
   	   
   	   If (err1 > 0) Then
   	       dim1+= inc1
   	       err1 -= d5x2
   	   EndIf 
   	   
   	   If (err2 > 0) Then 
   	       dim2+= inc2
   	       err2 -= d5x2
   	   EndIf 
   	   
   	   If (err3 > 0) Then 
   	       dim3+= inc3
   	       err3 -= d5x2
   	   EndIf 
   	   
   	   If (err4 > 0) Then 
   	       dim4+= inc4
   	       err4 -= d5x2
   	   EndIf 
   	   
   	   If (err5 > 0) Then 
   	       dim6+= inc6
   	       err5 -= d5x2
   	   EndIf 
   	   
   	   err1 += d1x2
   	   err2 += d2x2
   	   err3 += d3x2
   	   err4 += d4x2
   	   err5 += d6x2
   	   
   	   dim5+= inc5
   	   
   	   EjeX=dim1
   	   EjeY=dim2
   	   EjeZ=dim3
   	   EjeV=dim4
   	   EjeW=dim5
   	   EjeD=dim6
   	   
   	   InverseK

   	Next
   
   EndIf
   
   	If (Ad6> Ad1) And (Ad6> Ad2) And (Ad6> Ad3) And (Ad6> Ad4) And (Ad6> Ad5) Then 
   	
   	err1 = d1x2 - Ad6
   	err2 = d2x2 - Ad6
   	err3 = d3x2 - Ad6
   	err4 = d4x2 - Ad6
   	err5 = d5x2 - Ad6
   	
   	For Conta = 1 To Ad6
   	   
   	   If (err1 > 0) Then
   	       dim1+= inc1
   	       err1 -= d6x2
   	   EndIf 
   	   
   	   If (err2 > 0) Then 
   	       dim2+= inc2
   	       err2 -= d6x2
   	   EndIf 
   	   
   	   If (err3 > 0) Then 
   	       dim3+= inc3
   	       err3 -= d6x2
   	   EndIf 
   	   
   	   If (err4 > 0) Then 
   	       dim4+= inc4
   	       err4 -= d6x2
   	   EndIf 
   	   
   	   If (err5 > 0) Then 
   	       dim5+= inc5
   	       err5 -= d6x2
   	   EndIf 
   	   
   	   err1 += d1x2
   	   err2 += d2x2
   	   err3 += d3x2
   	   err4 += d4x2
   	   err5 += d5x2
   	   
   	   dim6+= inc6
   	   
   	   EjeX=dim1
   	   EjeY=dim2
   	   EjeZ=dim3
   	   EjeV=dim4
   	   EjeW=dim5
   	   EjeD=dim6
   	
   	   InverseK
   	   
   	Next
   	
   EndIf

   Xold=EjeX
   Yold=EjeY
   Zold=EjeZ
   Vold=EjeV
   Wold=EjeW
   Dold=EjeD
   
   InverseK
   
   StandBy=0
   
End Sub