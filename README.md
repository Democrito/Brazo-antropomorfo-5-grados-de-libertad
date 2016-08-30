# Brazo-antropomorfo-5-grados-de-libertad

"BrazoRobot.bas" y DiezCubos.bas" son los códigos fuentes en lenguaje de programación FreeBasic de los dos simuladores de brazo robot que está en esta web: https://sites.google.com/site/proyectosroboticos/Descargar-Simuladores

"Emulador RV-2AJ compatible COSIMIR.bas" es el código fuente de un simulador parecido a los anteriores, en el que además te permite leer ficheros de extensión ".POS" del Cosimir. Más información: https://sites.google.com/site/proyectosroboticos/Descargar-Simuladores/simulador-rv-2aj

@.) Descarga todo dándole a "Clone or download" (arriba a la derecha, botón de color verde). Le das a la opción descargar ZIP y se descargará. Abres el zip y tomas la carpeta que está en su interior y la pones en el escritorio o donde te sea más cómodo. Abres la carpeta y allí tienes tres códigos fuentes con extensión .bas. Tomas el que te interese y si tienes instalado el compilador lo podrás compilar y ejecutar.

@.) Si al compilar te da   "error 58"   has de eliminar el fichero  "windows.bi"  de dicha carpeta;  ese
fichero lo tengo puesto para poder ser compatible con versiones antiguas del compilador de FreeBasic.

@.) Según  la  versión  del  compilador,  en  las funciones    "MultiKey()"    puede dar problemas.   Si
te da fallo en esa función tendrías que cambiar todos los "FB.SC_" por "SC_", o al revés (según el caso),
y asunto resuelto.

Ejemplo:

If MultiKey(FB.SC_A) Then Xreal=Xreal-1

por

If MultiKey(SC_A) Then Xreal=Xreal-1

Así con todas las funciones "MultiKey()".

@.) Los ficheros .plt son ejemplos para que el brazo robot dibuje su contenido.

@.) Los ficheros .xyz son ejemplos de movimientos preprogramados; sólo son compatibles con "BrazoRobot.bas",
los otros dos simuladores no sabrán leerlos; si este es tu caso bórralos.

@.) Información complementaria: https://sites.google.com/site/proyectosroboticos/Descargar-Simuladores

