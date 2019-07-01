:userdoc.
:docprof toc=123456.
:title.Ayuda de NewView
.* ************************************************************
.* Subject: Ayuda de NewView
.* Version:
.* Copyright: Copyright 2004 Aaron Lawrence
.* Copyright: Copyright 2006-2007 Ronald Brill
.* Author: Aaron Lawrence
.* Translator: Alfredo Fern†ndez D°az
.* ************************************************************
.*
.*
.* ************************************************************
.* Introduction
.* ************************************************************
:h1 res=30000 id='Introduction'.Introducci¢n
:i1 id=30001.support
:p.:artwork runin name='..\images\NewView.bmp'.:hp2.≠Bienvenidos a NewView!:ehp2.
.br
:artwork name='NewViewW.bmp' align='center'.
:p.NewView es un programa para leer archivos de ayuda de OS/2 (o ArcaOS o eComStation).
:p.:link reftype=hd refid='Support'.Soporte y licencia:elink.
:p.:link reftype=hd refid='Using'.Utilizar NewView:elink.
:p.:hp1.Para hacer que este archivo deje de aparecer cuando inicie NewView sin
especificar un archivo de ayuda, cambie los ajustes de la pesta§a ÆGeneralØ, en
el men£ Herramientas &mdash. Opciones.:ehp1.
:p.:hp2.Historia:ehp2.
:p.NewView sustituye al programa original de IBM suministrado con OS/2.
:p.Mejora sobre muchos aspectos de View, con un interfaz moderno, f†cil de usar,
m†s opciones, y nuevas funciones que View simplemente no ten°a.

:h2 res=1 id='Support'.Soporte y licencia
:i1 id=30002.license
:i2 refid=30001.Support and Licensing
:i2 refid=30001.Introduction
:i1 id=30003.bugs
:i1 id=30004.source code
:p.:hp2.Soporte y licencia:ehp2.
:p.NewView es Copyright 1999&ndash.2006 Aaron Lawrence. TambiÇn se encuentra bajo
la licencia p£blica GNU, lo que significa que tiene vd. derecho a obtener el
c¢digo fuente.
:p.Desde 2006 Ronald Brill mantiene el producto. NewView es un proyecto de Netlabs.
:p.Consulte el archivo Readme.txt para m†s detalles tÇcnicos.
:p.Consulte el archivo Changes.txt para el historial de cambios de NewView.
:p.Si encuentra £til NewView, por favor env°e un correo y/o haga una donaci¢n
para apoyar su ulterior desarrollo. ≠Siempre es agradable tener noticias suyas!
:ul.
:li.Sugerencias, cumplidos, o informes de errores:
.br
http&colon.//svn.netlabs.org/newview
:li.Traducir NewView a su idioma.
:li.Una donaci¢n a Netlabs
.br
http&colon.//www.mensys.nl
:eul.

:p.:hp2.Informes de errores:ehp2.

:p.Si necesita informar de un cierre inesperado del programa u otro problema,
sea tan espec°fico como le resulte posible sobre quÇ archivos estaba utilizando,
lo que estaba haciendo, etc. Si est† disponible, POR FAVOR incluya el archivo
newview.log. Este archivo de registro estar† BIEN
.br
 &mdash. en el mismo directorio que el mismo NewView, BIEN
.br
 &mdash. en el directorio al que apunte la variable de entorno LOGFILES (t°picamente
 en eCS 1.1+).
.br
Si es espec°fico de ciertos archivos de ayuda en particular, entonces
env°emelos, a no ser que sean muy grandes (m†s de 1MB).

:p.La mayor parte de la informaci¢n siguiente est† en el archivo newview.log,
pero ser°a de ayuda que pudiera incluirla para verificarla:
:ul.
:li.Versi¢n de NewView (Ayuda &mdash. Informaci¢n del producto)
:li.Nombres de archivo de los archivos de ayuda.
:li.Una captura de pantalla puede ser £til, si el problema es que el programa
muestra informaci¢n incorrecta o una imagen corrupta.
:eul.

:p.:hp2.®Por quÇ no funciona correctamente mi archivo de ayuda?:ehp2.

:p.Algunas de las funciones menos usadas del View original no se han copiado. Esto
es porque a£n no he llegado a ello, o simplemente el tiempo necesario no merece la
pena. Algunos ejemplos son metaarchivos, °ndices de sin¢nimos, la API de control
de la aplicaci¢n al completo, y cosas as°.

:p.Por desgracia, parece que al menos un programador ha usado cada una de estas
funciones, por lo que ocasionalmente puede encontrarse con un archivo que no cargue
o no funcione correctamente.

:h1 res=2 id='Using'.Utilizar NewView
:p.:hp2.Utilizar NewView:ehp2.
:p.Una vez que haya :link reftype=hd refid='OpeningFiles'.abierto un archivo:elink.,
puede vd. leerlo de varias formas.
:p.Puede leer la :link reftype=hd refid='contents'.tabla de contenido:elink., usar
el :link reftype=hd refid='Index'.°ndice alfabÇtico:elink., o
:link reftype=hd refid='search'.buscar:elink..
:p.Para leer el archivo de ayuda simplemente como un libro de papel, utilice los
botones ÆAnteriorØ :artwork runin name='..\images\previous.bmp'. y ÆSiguienteØ
:artwork runin name='..\images\next.bmp'. para recorrer todos los temas.
:p.TambiÇn puede leer el archivo de ayuda como p†ginas web, utilizando los botones
ÆAtr†sØ :artwork runin name='..\images\back.bmp'. y ÆAdelanteØ :artwork runin name='..\images\forward.bmp'.
para volver a donde se encontraba antes, o para rehacer sus pasos.
:p.Los colores y parte del comportamiento  de NewView pueden configurarse desde el
men£ Herramientas &mdash. Opciones.
:p.TambiÇn puede :link reftype=hd refid='notes'.hacer anotaciones:elink. o :link
reftype=hd refid='bookmarks'.colocar marcadores:elink. en cada tema de ayuda.

:h1 res=3 id='OpeningFiles'.Abrir archivos
:i1 id=30005.open
:p.:hp2.Abrir archivos de ayuda:ehp2.
:p.Puede abrir un archivo de ayuda de cualquiera de las siguientes formas:
:p.&mdash. Haciendo doble clic sobre :link reftype=hd refid='HelpIcons'.un icono de ayuda:elink.
que ya exista.
:p.&mdash. Tecleando Æview :hp1.archivo:ehp1.Ø desde el :link reftype=hd
refid='CommandLine'.indicador de mandatos:elink.
:p.&mdash. Pulsando el bot¢n ÆAbrirØ :artwork runin name='..\images\open.bmp'. desde NewView
:p.&mdash. Volviendo a cargar un archivo de ayuda abierto recientemente desde el men£ ÆArchivoØ
:p.&mdash. Arrastrando un archivo de ayuda desde el Escritorio sobre el programa
:p.Una vez cargado el archivo, deber°a ver la :link reftype=hd
refid='contents'.tabla de contenido:elink. y el primer tema de ayuda.
:p.:hp2.Nota&colon.:ehp2. se asume que ha instalado NewView como sustituto del View
original. Si no lo ha hecho, entonces los iconos de ayuda existentes y el indicador
de mandatos pueden comportarse de forma diferente a la descrita.
:p.:hp2.Cargar varios archivos a la vez:ehp2.
:p.NewView puede cargar varios archivos a la vez, present†ndolos como si fueran un
£nico documento, e interpretar variables de entorno como nombres de archivo.
:p.Por ejemplo, con la documentaci¢n del OS/2 Developer's Toolkit:
.br
  NewView cpref
.br
carga ÆControl Program Guide and ReferenceØ. CPREF es una variable de entorno que se
establece en el archivo config.sys y consiste en ÆCP1+CP2+CP3Ø, lo que indica a NewView
(o View) que cargue los archivos de ayuda CP1, CP2 y CP3. Estos archivos se buscan
en la v°a de acceso especificada por dos :link
reftype=hd refid='L_EnvironmentVariables'.variables de entorno:elink..*
:p.Todos los archivos se cargan y anexan a los otros.
:p.La capacidad de cargar as° varios archivos a la vez puede ser £til por varios
motivos. Por ejemplo, 4OS/2 (un sustituto de CMD.EXE) la utiliza para para cargar su
propia ayuda adem†s de la ayuda original de CMD. Puede hacer esto vd. mismo con los
archivos que quiera.
:p.Puede cargar varios archivos en el di†logo ÆAbrir archivoØ pulsando Control
o May£sculas para seleccionar varios archivos.
:p.Cuando haga clic en un enlace a un archivo de ayuda diferente, NewView carga
el otro archivo sin cerrar los archivos que tenga abiertos en ese momento.
:p.En cualquier momento puede averiguar quÇ archivos hay abiertos utilizando
Archivo &mdash. Informaci¢n.
:p.:hp2.Cargar archivos adicionales:ehp2.
:p.Puede marcar la casilla ÆMantener abiertos los archivos actualesØ del di†logo
ÆAbrir archivoØ, y NewView abrir† los archivos que haya seleccionado sin cerrar
los que haya abiertos en ese momento.
:p.:hp2.Arrastrar y soltar:ehp2.
:p.Puede arrastrar y soltar archivos .INF o .HLP sobre NewView y los abrir†. Si
mantiene pulsada la tecla May£sculas, los abrir† sin cerrar
los que haya abiertos en ese momento.
:p.Puede soltar los archivos en cualquiera de las †reas principales de contenido,
como las ventanas Contenido o ÷ndice, o una ventana en que se muestre un tema de
ayuda del archivo.
:p.:hp2.Nota:ehp2. Algunos enlaces que conectan archivos entre s°, s¢lo
funcionar†n si se ha cargado el conjunto correcto de archivos.

:h2 res=17 id='HelpIcons'.Iconos de ayuda
:p.:hp2.Iconos de ayuda:ehp2.
:p.Los iconos de ayuda del Escritorio son normalmente Æobjetos de programaØ con el
nombre del programa especificado como Æview.exeØ y el nombre de los archivos de ayuda
como par†metros.
:p.Algunos programas crean autom†ticamente estos iconos durante la instalaci¢n.
:p.Puede crear estos iconos vd. mismo utilizando el modelo ÆPrograma nuevoØ del
Escritorio. Consulte la ayuda del Escritorio para m†s informaci¢n.
:p.Si crea iconos arrastrando archivos de ayuda al Escritorio, no podr† darles
un nombre que tenga sentido, porque al hacerlo cambiar°a el nombre del archivo,
lo que a su vez podr°a evitar que los programas ancuentren el archivo de ayuda.
Por tanto, se recomienda utilizar objetos de programa como medio de crear iconos
de ayuda.

:h1 res=200 id='NavigationPanel'.Pesta§as del panel de navegaci¢n
:p.:hp2.Pesta§as del panel de navegaci¢n:ehp2.

:p.El panel de la izquierda tiene varias pesta§as para moverse a travÇs
del archivo de ayuda actual de diferentes formas:
.br
:artwork runin name='tabs.bmp'.

:p.:ul compact.
:li.:link reftype=hd refid='contents'.Contenido:elink.
:li.:link reftype=hd refid='Index'.÷ndice:elink.
:li.:link reftype=hd refid='search'.B£squeda:elink.
:li.:link reftype=hd refid='notes'.Notas:elink.
:eul.

:p.Puede ocultar este panel para ganar espacio, con el bot¢n
:artwork name='..\images\navigator.bmp' runin. o seleccionando Ver &mdash. Mostrar panel
izquierdo en el men£, o pulsando Alt+P. Haga lo mismo para mostrarlo de nuevo.
:p.Puede hacer que el panel de navegaci¢n deje de aparecer al abrir un archivo
de ayuda en Herramientas &mdash. Opciones &mdash. Pesta§a ÆGeneralØ.
:p.:hp2.Nota: :ehp2. Muchos programas eligen mostrar la tabla de contenido cuando
abren su archivo de ayuda; en este caso el panel se abre de forma autom†ticamente,
ignorando su selecci¢n.

:h2 res=4 id='contents'.Contenido
:i1 id=30006.contents
:p.:hp2.Contenido:ehp2.
:p.La mayor°a de los archivos de ayuda tienen una tabla de contenido que muestra
los temas del archivo de forma jer†rquica o Æen †rbolØ. Habitualmente, Çsta
es la primera vista que se muestra cuando abre vd. un archivo.
:p.Puede vd. expandir o colapsar ramas del †rbol pulsando los botones + y -, o
pulsando la barra espaciadora del teclado.
:p.Para ver un tema desde la tabla de contenido, simplemente haga clic sobre
ella. TambiÇn puede moverse a travÇs del contenido utilizando las teclas del
cursor.
:p.Para moverse a travÇs de :hp2.todos:ehp2. los temas del †rbol de contenido, por orden,
puede usar las teclas Ctrl+Arriba o Ctrl+Abajo, o los botones
ÆAnteriorØ :artwork runin name='..\images\previous.bmp'. y
ÆSiguienteØ :artwork runin name='..\images\next.bmp'.&per. êsta es una forma de tratar
el archivo como si fuera un libro normal, leyendo cada p†gina.
:p.TambiÇn puede ojear la tabla de contenido completa utilizando ÆExpandir todoØ
en el men£ ÆVerØ. Esto expande todas las ramas de la tabla de contenido de forma
que pueda examinarla r†pidamente. No obstante, normalmente es m†s f†cil utilizar
:link reftype=hd refid='search'.B£squeda:elink. o :link reftype=hd
refid='Index'.÷ndice:elink. para esto.

:h2 res=5 id='Index'.÷ndice
:p.:hp2.÷ndice:ehp2.
:p.La pesta§a ÷ndice contiene un listado alfabÇtico de temas o palabras
clave del archivo de ayuda. Puede buscar r†pidamente a travÇs de ella simplemente
escribiendo los primeros caracteres de la palabra que quiera buscar. NewView salta
hasta la primera coincidencia en el °ndice de forma autom†tica. Para ver el tema
seleccionado, pulse enter.
:p.:hp2.Notas:ehp2.
:p.Los archivos de ayuda pueden contener o no un °ndice ÆoficialØ. El °ndice
debe ser creado a mano, de modo que su utilidad (para el View original) depende
estrictamente de cu†nto trabajo pusiera en Çl el autor. Puede que ni siquiera
haya uno.
:p.No obstante, puede porporcionarse un °ndice util simplemente listando
alfabÇticamente los t°tulos de cada tema, y esto es lo que hace NewView.
A continuaci¢n fusiona el °ndice original (si lo hay) con la lista de t°tulos
de los temas.
:p.Si por alguna raz¢n esto no le gusta, puede desactivar el mecanismo en
Herramientas &mdash. Opciones &mdash. pesta§a Æ÷ndiceØ.

:h2 res=6 id='search'.B£squeda
:p.:hp2.Buscar:ehp2.
:p.Buscar es una forma r†pida de encontrar informaci¢n cuando no se sabe por
d¢nde empezar. Simplemente vaya a la pesta§a B£squeda, teclee algunas palabras
relacionadas y pulse el bot¢n Buscar.
:p.Ver† una lista de todos los temas que contienen esa palabra, o palabras
parecidas, con las mejores coincidencias al principio. La mejor coincidencia
de todas se mostrar† autom†ticamente.
:p.Las palabras que coincidan con su b£squeda aparecer†n resaltadas en el tema.
:p.:hp2.B£squeda global:ehp2.
:p.TambiÇn puede buscar en todos los archivos de ayuda de su sistema utilizando la
:link reftype=hd refid='GlobalSearch'.b£squeda global:elink. en Herramientas &mdash.
Buscar en todos los archivos de ayuda.
:p.:hp2.B£squeda de frases:ehp2.
:p.Si quiere buscar una expresi¢n compuesta por m†s de una palabra, escr°bala
rodeada de dobles comillas, por ejemplo "os/2 warp".
:p.:hp2.Control de coincidencias:ehp2.
:p.NewView le permite un control m†s fino de la b£squeda.
:p.+ indica una palabra que :hp2.debe:ehp2. aparecer en los resultados de la b£squeda
:p.- indica una palabra que :hp2.no debe:ehp2. aparecer
:p.NewView siempre busca coincidencias parciales de las palabras. Es decir, si busca
ÆwinØ NewView tambiÇn encontrar† ÆwindowØ y ÆshowingØ. No obstante, cuanto mejor sea
la coincidencia, mayor ser† su rango en la b£squeda.
:p.:hp2.C¢mo establece NewView los rangos de los resultados:ehp2.
:p.NewView asigna un rango a los resultados de la b£queda de varias formas:
:p.&mdash. coincidencia m†s pr¢xima a una palabra completa
:p.&mdash. n£mero de palabras encontradas en un tema
:p.&mdash. las coincidencias aparecen en el t°tulo
:p.&mdash. las coincidencias aparecen en una entrada del °ndice

:h2 res=7 id='notes'.Notas
:p.:hp2.A§adir y utilizar notas:ehp2.
:p.NewView le permite a§adir notas (anotaciones) a sus archivos de ayuda.
:p.Para a§adir una nota, simplemente pulse con el rat¢n para dejar el cursor
donde quiera dejar su nota, y pulse el bot¢n ÆNotaØ :artwork runin
name='..\images\note.bmp'., teclee el texto y pulse Guardar. El texto se
insertar† en el tema en otro color (por omisi¢n en verde; puede
cambiar esto en Herramientas &mdash. Opciones &mdash. Colores).
:p.Para editar o eliminar una nota, pulse en el texto coloreado de la nota;
entonces puede editar el texto o pulsar Borrar para deshacerse de Çl.
:p.TambiÇn puede revisar todas las anotaciones que haya hecho en el o los
archivos de ayuda actuales yendo a la pesta§a Notas; esto le permite a§adir,
editar y borrar, y tambiÇn saltar a los temas que contengan sus notas.
:p.Las notas se guardan en un archivo con la extensi¢n .nte, en el mismo
directorio que el archivo de ayuda al que se refieren.
:p.:hp2.Nota&colon.:ehp2. si se cambia un archivo de ayuda (por ejemplo, al
actualizar un programa) las notas no aparecer†n en el lugar correcto; no
obstante, a£n podr† leerlas desde la pesta§a Notas.

:h1 res=18 id='GlobalSearch'.B£squeda global
:p.:hp2.B£squeda global:ehp2.
:p.Puede buscar en todos los archivos de ayuda de su sistema, pulsando el bot¢n
B£squeda global :artwork runin name='..\images\search.bmp'., utilizando
Herramientas &mdash. ÆBuscar en todos los archivos de ayudaØ en el men£, o pulsando
Ctrl+S.
:p.Esta b£squeda funciona de forma similar a la :link reftype=hd refid='search'.
b£squeda en un archivo:elink., pero tambiÇn le indica en quÇ archivo de ayuda
se encontraron los resultados de la b£squeda.
:p.Estas b£squedas pueden tardar algo de tiempo, dependiendo de lo que busque.
Puede detener vd. la b£squeda en cualquier momento.
:p.:hp2.D¢nde busca NewView:ehp2.
:p.La opci¢n por omisi¢n es buscar los archivos de ayuda en las v°as de acceso
para ayuda del sistema, especificadas por las :link reftype=hd
refid='L_EnvironmentVariables'.variables de entorno:elink. BOOKSHELF y HELP.
:p.Puede elegir otros lugares en los que buscar eligiÇndolos de la lista desplegable,
o personalizar la lista pulsando el bot¢n Seleccionar.
:p.:hp2.Buscar en: V°as de acceso est†ndar:ehp2.
:p.êsta es la opci¢n por omisi¢n y se buscar† en BOOKSHELF y HELP tal como se
especific¢ m†s arriba.
:p.Pulsar el bot¢n Seleccionar le permitir† elegir cu†les de los directorios en
las v°as de acceso de ayuda del sistema se utilizar†n. Pulse sobre cada elemento
de la lista para seleccionarlo o deseleccionarlo. DespuÇs de elegir esto, esta
opci¢n aparecer† como ÆV°as de acceso seleccionadasØ.
:p.:hp2.Buscar en: Todos los discos duros:ehp2.
:p.Esta opci¢n buscar† en todos los discos duros (no extra°bles) de su sistema. Puede
pulsar ÆSeleccionarØ para personalizar la lista.
:p.Buscar en todas las unidades puede encontrar m†s archivos de ayuda, pero
podr°a ser mucho m†s lento que s¢lo las v°as de acceso de ayuda.
:p.:hp2.Buscar en: V°as de acceso seleccionadas:ehp2.
:p.Si ya ha seleccionado algunas v°as de acceso particulares para la b£squeda,
puede pulsar en ÆSeleccionarØ para personalizar la lista de nuevo.
:p.:hp2.Buscar en: Lista de directorios:ehp2.
:p.En el di†logo ÆSeleccionar directoriosØ, pulsar el bot¢n ÆA§adirØ le permitir†
a§adir uno o m†s directorios a la lista de b£squeda.
:p.Seleccione la unidad y directorio utilizando los controles que aparecen, y pulse
Æ< A§adir directorioØ para a§adir el directorio elegido. Puede hacer esto tantas
veces como le plazca. Elija Æcon subdirectoriosØ si quiere que tambiÇn se busque
en los subdirectorios del directorio seleccionado. En este caso, se mostrar† Æ...Ø
al final del nombre del directorio.
:p.DespuÇs de a§adir un directorio personalizado as°, la opci¢n de b£squeda
aparecer† como ÆLista de directoriosØ.
:p.:hp2.Nota&colon.:ehp2. Si a§ade un directorio personalizado a las v°as de acceso estandar o una
selecci¢n de ellas, la lista se convierte en una lista personalizada, y no podr†
volver a seleccionar ÆV°as de accesoØ. Para volver a las v°as de acceso de ayuda
originales, elija ÆV°as de acceso est†ndarØ y vuelva a pulsar Seleccionar.
:p.:hp2.Buscar en: Introducir localizaci¢n:ehp2.
:p.Puede teclear una v°a de acceso en el campo de entrada ÆBuscar en: Ø. A§ada
Æ...Ø al final del nombre del directorio si quiere que tambiÇn se busque en los
directorios.
:p.Ejemplo:
:p. Buscar en: [ E&colon.\misdocs\... ]
:p.Se buscar†n archivos de ayuda en E&colon.\misdocs\ y cualesquiera subdirectorios.

:h1 res=8 id='bookmarks'.Marcadores
:p.:hp2.Marcadores:ehp2.
:p.NewView le permite colocar marcadores en temas concretos del archivo de ayuda actual.
Simplemente pulse el bot¢n Marcador :artwork runin name='..\images\bookmark.bmp'. para
a§adir el tema de ayuda actual a sus marcadores..
:p.Para acceder a un marcador, vaya al men£ ÆMarcadoresØ, y pulse en el marcador
que quiera abrir.
:p.Puede ver o eliminar todos los marcadores  eligiendo ÆAdministrarØ en el men£
ÆMarcadoresØ. Esta ventana puede permanecer abierta mientras lee, para que pueda
recorrer r†pidamente sus marcadores.
:p.:hp2.Notas:ehp2.
:p.Los marcadores de NewView recuerdan quÇ ventanas con temas de ayuda hay abiertas,
si hay m†s de una.
:p.Los marcadores se guardan en un archivo con la extensi¢n .bmk, en el mismo directorio
que el archivo de ayuda al que corresponden.

:h1 res=100 id='InternetLinks'.Enlaces a Internet
:p.:hp2.Enlaces a Internet:ehp2.
:p.Al pulsar sobre un URL como http&colon.//www.google.com, NewView inicia
su navegador por omisi¢n.
:p.Este navegador se especifica en las opciones del sistema operativo, no en
el propio NewView. Para configurarlo, puede abrir un objeto URL del Escritorio,
editar la v°a de acceso al navegador en la pesta§a :hp2.Examinador:ehp2., y pulsar
Establecer por omisi¢n. Alternativamente, descargue el programa ConfigApps
de Hobbes
.br
 http&colon.//hobbes.nmsu.edu/cgi-bin/h-search?key=configapps
.br
:p.Los navegadores tambiÇn pueden tener la capacidad de establecerse a s° mismos
como opci¢n por omisi¢n, bien durante la instalaci¢n, bien en las preferencias.
:p.NewView tiene en cuenta la configuraci¢n establecida en Internet&mdash.Aplicaciones&mdash.Integraci¢n.
Los enlaces de correo, grupos de noticias y FTP se pasan tambiÇn al programa
establecido. Si no hay un programa establecido para un tipo concreto de URL, entonces
se pasan los enlaces al navegador.
:p.:hp2.Nota para los autores de archivos de ayuda:ehp2.
:p.El View original no entend°a URLs ni enlaces de correo, de forma que la £nica
manera de hacerlos funcionar era un enlace de aplicaci¢n a, por ejemplo,
Ænetscape.exeØ con los par†metros correctos.
:p.NewView transforma los enlaces de aplicaci¢n a ÆnetscapeØ, ÆexploreØ o ÆmozillaØ
en enlaces al navegador por omisi¢n.
:p.TambiÇn autodetecta URLs con las formas:
:p. http&colon.//x  https&colon.//x ftp&colon.//x
:p. mailto&colon.x  news&colon.x
:p.TambiÇn se detecta texto con aspecto de URL, incluso sin el prefijo del protocolo:
:p. www.a.b &mdash. navegador
:p. ftp.a.b &mdash. ftp
:p. a@b.c &mdash. correo
:p.donde a, b y c son cadenas alfanumÇricas cualesquiera..
:p.No es necesario hacer nada para que NewView los reconozca.

:h1 res=9 id='CommandLine'.Par†metros de ejecuci¢n
:p.:hp2.Par†metros del indicador de mandatos:ehp2.
:p.Cuando se ejecuta NewView se le pueden suministrar varios par†metros, por
ejemplo desde el indicador de mandatos. Ninguno de ellos es estrictamente
necesario.
:p.:hp2.NewView [opciones] [<archivo> [<texto que buscar>]]:ehp2.
:p.Si NewView se instala como sustituto de View, entonces el mandato deber†
comenzar con ÆviewØ en vez de ÆnewviewØ.
:p.:link reftype=hd refid='CommandLineExamples'.Ejemplos:elink.
:p.:hp2.<archivo>:ehp2.
:p.El archivo que NewView debe cargar. Puede cargar varios archivos de una sola vez
utilizando archivo1+archivo2, etc.
:p.Si no especifica una v°a de acceso, entonces los archivos se buscan en las
v°as de acceso especificadas en
:link reftype=hd refid='L_EnvironmentVariables'.BOOKSHELF y HELP:elink..
:p.Si la v°a de acceso o el nombre de archivo contienen espacios, deber† especificar
la v°a de acceso completa entre comillas dobles.
:p.:hp2.<texto que buscar>:ehp2.
:p.Se buscar† este texto en los t°tulos d elos temas y las entradas del °ndice.
Esto :hp2.no:ehp2. es lo mismo que la b£squeda normal, por compatibilidad con el
View original. Para hacer una b£squeda propiamente dicha utilice la opci¢n /s
(ver m†s abajo). Para m†s detalles, consulte :link reftype=hd
refid='CommandLineTopicSearch'.Buscar temas desde el indicador de mandatos:elink..
:p.:hp2.Opciones:ehp2.
:p.:hp2./s:ehp2.
:p.Tras abrir el archivo, se realiza una :link reftype=hd refid='search'.b£squeda:elink.
del texto especificado (una verdadera b£squeda en todo el texto, no la b£squeda por
omisi¢n en los t°tulos de los temas). El resultado es el mismo que en la
:link reftype=hd refid='search'.b£squeda en el panel de navegaci¢n:elink.&per.
:p.Ejemplo&colon.
.br
Para buscar ÆcopiarØ en todo el documento ÆcmdrefØ, puede ejecutar
:cgraphic.
  newview /s cmdref copiar
:ecgraphic.
NewView es lo bastante inteligente para manejar varias palabras (como la
:link reftype=hd refid='search'.b£squeda en el panel de navegaci¢n:elink.)&per.
êsta es una b£squeda del tipo O l¢gico (apariciones de una palabra u otra).
:cgraphic.
  newview /s cmdref net access
:ecgraphic.
Para realizar una b£squeda del tipo Y l¢gico (apariciones simult†neas),
ponga la frase buscada entre comillas dobles&per.
:cgraphic.
  newview /s cmdref "net access"
:ecgraphic.
:p.:hp2./g:ehp2.
:p.Se realiza una :link reftype=hd refid='GlobalSearch'.b£squeda global:elink. del
texto especificado, en todos los archivos de ayuda de su sistema..
:p.Ejemplo&colon.
.br
Para buscar ÆcopiarØ en todos los archivos de ayuda, use
:cgraphic.
  newview /g copiar
:ecgraphic.
Introduzca el nombre de archivo como primer par†metro si quiere abrir un archivo
de ayuda antes de que comience la b£squeda&per.
:cgraphic.
  newview /g cmdref copy
:ecgraphic.
:p.:hp2./?:ehp2. o :hp2./h:ehp2. o :hp2./help:ehp2.
:p.Muestra ayuda sobre las opciones en el indicador de mandatos.
:p.Consulte tambiÇn: :link reftype=hd refid='AdvancedParameters'.Par†metros
avanzados:elink.

:h2 res=13 id='CommandLineTopicSearch'.Buscar temas desde el indicador de mandatos
:p.:hp2.Buscar temas desde el indicador de mandatos:ehp2.
:p. view <archivo> <tema>
:p.Los par†metros de b£squeda especificados en un indicador de mandatos
copian el comportamiento del View original.
:p.No se busca el texto en los temas, s¢lo en los t°tulos y las entradas del
°ndice. Esto hace la b£squeda menos £til para las personas, pero algunos programas
la utilizan para referirse a los temas de ayuda de forma predecible.
:p.Puede utilizar varias palabras.
:p.Lo que se comprueba es si:
:p.&mdash. el t°tulo de los temas empieza por el texto
:p.&mdash. las entradas del °ndice empiezan por el texto
:p.&mdash. el t°tulo de los temas contiene el texto
:p.&mdash. las entradas del °ndice contienen el texto.
:p.Los programadores deber°an asegurarse de que se pueda encontrar el documento
esperado si se utiliza esta tÇcnica para identificar temas al utilizar NewView o
el View original.

:h2 res=14 id='AdvancedParameters'.Par†metros avanzados
:p.:hp2.Par†metros avanzados:ehp2.
:p.Los siguientes par†metros de indicador de mandatos est†n orientados £nicamente
a programadores, pero pueden utilizarse con cualquier prop¢sito.
:p.:hp2./lang&colon.<idioma>:ehp2.
:p.Carga el idioma especificado. Se ignora la opci¢n por omisi¢n, que es elegida
seg£n la variable de entorno LANG. Por ejemplo,
:cgraphic.
newview cmdref /lang&colon.en
:ecgraphic.
carga el inglÇs. Consulte readme.txt para m†s informaci¢n.
:p.:hp2./pos&colon.<izquierda>,<abajo>,<ancho>,<alto>:ehp2.
:p.Establece la posici¢n y el tama§o dados para la ventana principal del programa.
Todos los valores deben ser especificados. Coloque una :hp2.P:ehp2. despuÇs de
un n£mero para especificar un porcentaje. Por ejemplo:
:cgraphic.
newview /pos&colon.10P,10P,80P,80P
:ecgraphic.
centra la ventana y le da un 80% de las dimensiones de la pantalla.
:p.:hp2./title&colon.<t°tulo de la ventana>:ehp2.
:p.Establece el texto especificado como t°tulo de la ventana de NewView,
ignorando lo que aparezca en el archivo de ayuda. El texto ÆAyuda - Ø siempre
se insertar† antes del texto especificado, a no ser que dicho texto sea ÆAyudaØ,
en cuyo caso el t°tulo pasar† a ser sencillamente ÆAyudaØ. Esto pretende asegurar
que las ventanas de ayuda sean evidentes como tales en la lista de ventanas.
:p.Si necesita especificar varias palabras, rodee la opci¢n entera con comillas,
por ejemplo:
:cgraphic.
newview cmdref "/title&colon.Ayuda para el indicador de mandatos"
:ecgraphic.

:h2 res=15 id='CommandLineExamples'.Ejemplos para el indicador de mandatos
:p.:hp2.Ejemplos para el indicador de mandatos:ehp2.
:p.Los siguientes ejemplos asumen que NewView se ha instalado como sustituto completo
y por tanto view es en realidad NewView..
:p.:hp2.view cmdref:ehp2.
:p.Abre el archivo cmdref.inf (Mandatos del sistema) desde la v°a de acceso de ayuda
del sistema.
:p.:hp2.view cmdref+os2ug:ehp2.
:p.Abre dos archivos, cmdref.inf y os2ug.inf (Gu°a del Escritorio de OS/2 Warp),
en la misma ventana.
:p.La tabla de contenido de os2ug.inf se a§ade al final de la tabla de contenido
de cmdref.inf. Los °ndices se combinan alfabÇticamente.
:p.:hp2.view c&colon.\os2\book\os2ug.inf:ehp2.
:p.Abre el archivo os2ug.inf en el directorio c&colon.\os2\book&per.
:p.:hp2.view "c&colon.\os2 book\os2ug.inf":ehp2.
:p.Escriba Æv°a de acceso\archivoØ entre comillas dobles si contiene caracteres
especiales (como espacios).
:p.:hp2.view cmdref dir:ehp2.
:p.Abre el archivo cmdref (Mandatos del sistema) y busca en los t°tulos y el °ndice
la palabra ÆdirØ. Mostrar† la p†gina de ayuda del mandato DIR.
:p.:hp2.view /s os2ug Escritorio:ehp2.
:p.Abre el archivo os2ug.inf y busca la palabra ÆEscritorioØ. Se muestra
la mejor coincidencia.
:p.:hp2.view /g permisos:ehp2.
:p.realiza una b£squeda de la palabra ÆpermisosØ en todos los archivos de ayuda.
:p.:hp2.set myhelp=cmdref+os2ug+rexx:ehp2.
.br
:hp2.view myhelp:ehp2.
:p.La primera l°nea establece una variable de entorno, MYHELP, que contiene los
nombres de tres archivos de ayuda. La segunda l°nea abre los tres archivos.

:h1 res=10 id='KeyboardShortcuts'.Teclas r†pidas
:p.:hp2.Teclas r†pidas:ehp2.
:p.La mayor°a de las teclas r†pidas son visibles en el men£, pero algunas no lo son.
Las otras teclas r†pidas son:
:p.:hp2.Alt+F4:ehp2. Salir
:p.:hp2.Ctrl+C:ehp2. Copiar el texto seleccionado al portapapeles
:p.:hp2.F7:ehp2. Atr†s
:p.:hp2.F8:ehp2. Adelante
:p.:hp2.Ctrl+Left:ehp2. Atr†s
:p.:hp2.F11:ehp2. Anterior seg£n contenido
:p.:hp2.F12:ehp2. Siguiente seg£n contenido
:p.
:p.:hp2.Teclas r†pidas visibles en el men£:ehp2.
:p.:hp2.Ctrl+O:ehp2. Abrir archivo
:p.:hp2.Ctrl+E:ehp2. Abrir especial
:p.:hp2.Ctrl+N:ehp2. Abrir una nueva ventana
:p.:hp2.Ctrl+P:ehp2. Imprimir el tema
:p.:hp2.F3:ehp2. Salir
:p.
:p.:hp2.Ctrl+A:ehp2. Seleccionar todo el texto del tema
:p.:hp2.Ctrl+Ins:ehp2. Copiar el texto seleccionado al portapapeles
:p.
:p.:hp2.Ctrl+F:ehp2. Buscar en el tema actual
:p.:hp2.Ctrl+G:ehp2. Repetir la £ltima b£squeda
:p.
:p.:hp2.Ctrl+S:ehp2. Abrir la herramienta de b£squeda global
:p.
:p.:hp2.Alt+C:ehp2. Cambiar a la pesta§a ÆContenidoØ
:p.:hp2.Alt+I:ehp2. Cambiar a la pesta§a Æ÷ndiceØ
:p.:hp2.Alt+S:ehp2. Cambiar a la pesta§a ÆB£squedaØ
:p.:hp2.Alt+N:ehp2. Cambiar a la pesta§a ÆNotasØ
:p.:hp2.Alt+P:ehp2. Mostrar u ocultar el panel izquierdo (con las pesta§as)
:p.:hp2.F5:ehp2. Expandir todo el contenido
:p.:hp2.F6:ehp2. Colapsar todo el contenido
:p.
:p.:hp2.Esc:ehp2. Atr†s
:p.:hp2.Ctrl+Right:ehp2. Adelante
:p.:hp2.Ctrl+Up:ehp2. Tema anterior seg£n contenido
:p.:hp2.Ctrl+Down:ehp2. Siguiente tema seg£n contenido
:p.
:p.:hp2.Ctrl+D:ehp2. Editar los marcadores
:p.:hp2.Ctrl+B:ehp2. Dejar marcador en el tema actual
:p.
:p.:hp2.Ctrl+M:ehp2. A§adir nota en la posici¢n del cursor
:p.
:p.:hp2.F1:ehp2. Ayuda de NewView

:h1 res=11 id='L_EnvironmentVariables'.Variables de entorno
:p.:hp2.Variables de entorno:ehp2.
:p.Ambas variables de entorno, :hp2.BOOKSHELF:ehp2. y :hp2.HELP:ehp2.,
definen v°as de acceso (listas de directorios) en las que buscar archivos
de ayuda. NewView utiliza ambas v°as de acceso sin distinci¢n.
:p.Se busca en estas v°as cuando vd.:
:ul.
:li.especifica en el indicador de mandatos un archivo de ayuda sin la v°a de acceso
:li.utiliza el men£ Archivo &mdash. Abrir especial
:li.realiza una :link reftype=hd refid='GlobalSearch'.b£squeda global:elink.
:eul.
:p.Puede a§adir directorios con archivos de ayuda a las v°as de acceso :hp2.HELP:ehp2.
o :hp2.BOOKSHELF:ehp2. modificando el archivo CONFIG.SYS. A§†dalos a ambas si quiere
que tambiÇn el viejo view pueda encontrar los archivos.
:p.:hp2.Otras variables de entorno:ehp2.
:p.La variable de entorno :hp2.LANG:ehp2. se examina para decidir el idioma en que
se mostrar† NewView por omisi¢n. (Anulada por :link reftype=hd refid='AdvancedParameters'.
el par†metro /lang del indicador de mandatos:elink..) Consulte el archivo readme.txt
de NewView para m†s informaci¢n sobre los idiomas.
:p.El directorio definido en :hp2.LOGFILES:ehp2. se utiliza para registrar colapsos
del programa u otra informaci¢n.
:p.Al iniciar, se buscan archivos de idioma en el subdirectorio ÆlangØ del
directorio definido por :hp2.OSDIR:ehp2..
:p.TambiÇn se buscan archivos de idioma en la v°a de acceso :hp2.ULSPATH:ehp2..

:h1 res=20 id='ForAuthors'.Para autores y programadores
:p.:hp2.Para autores y programadores:ehp2.
:p.Esta secci¢n contiene algunas notas para autores de documentos y
programadores.
:p.Consulte tambiÇn la secci¢n sobre reconocimiento de URLs en el tema
:link reftype=hd refid='InternetLinks'.Enlaces a Internet:elink..

:h2 res=12 id='WritingHelpFiles'.Escribir archivos de ayuda
:p.:hp2.Escribir archivos de ayuda de OS/2:ehp2.
:p.Los archivos de ayuda de OS/2 se generan utiizando el compilador IPF.
IPF significa Information Presentation Facility.
:p.El compilador IPF toma un archivo de texto escrito en un lenguaje que le indica
cosas como encabezados, enlaces, texto e im†genes, y produce un archivo .INF o .HLP.
:p.La forma oficial de obtener el compilador IPF (ipfc.exe) es del
OS/2 Developers Toolkit. êste se incluye de forma gratuita en eComStation
(http&colon.//www.ecomstation.com).
:p.Puesto que el lenguaje IPFC es tedioso (por ejemplo, toda la puntuaci¢n
debe indicarse mediante palabras clave especiales, como &amp.comma.) mucha gente
utiliza otras herramientas aparte del propio compilador IPF.
:p.Yo uso Vyperhelp (http&colon.//www.vyperhelp.com) puesto que es simple y gr†fico.
TambiÇn puede exportar a Ayuda de Windows, HTML y otros, a pesar de que s¢lo
est† disponible para OS/2. Ahora es gratuito.
:p.Otras opciones populares son:
:ul.
:li.HyperText/2 IPF Preprocessor
(http&colon.//www.clanganke.de/os2/sw/htext/) &mdash. preprocesa un lenguaje inicial
m†s simple y lo convierte en el dificultoso formato IPF. Gratuito.
:li.HyperMake (http&colon.//www.hypermake.com). Similar, pero tambiÇn produce
Ayuda de Windows y HTML.
:li.Sibyl (con el que se cre¢ NewView) incluye un preprocesador IPF.
:li.IPFEditor de PCS (http&colon.//www.pcs-soft.com/productipfe212.htm).
Probablemente el m†s completo, pero con un coste elevado.
.br
Nota: ≠NewView no soporta todo lo que IPFE puede hacer!
:eul.
:p.En el pasado hab°a otras muchas opciones. Las que est†n en la lista deber°an
estar a£n disponibles y disponer de alg£n tipo de soporte.

:h2 res=16 id='TopicResourceIDs'.IDs de recurso de los temas
:p.:hp2.IDs de recurso de los temas:ehp2.
:p.Los identificadores de recurso son utilizados por los autores de ayuda en l°nea
para aplicaciones para identificar los temas de ayuda. Las aplicaciones llaman
al Gestor de Ayuda especificando un ID de recurso, bien directamente utilizando el
mensaje HM_DISPLAY_HELP, o indirectamente a travÇs de tablas de ayuda a§adidas
a sus recursos, que el PM maneja autom†ticamente. El ID de recurso se almacena
en una tabla dentro del archivo de ayuda..
:p.Para los autores de documentos, NewView ofrece la capacidad de ver y buscar
los IDs de recurso.
:p.:hp2.Buscar por ID de recurso:ehp2.
:p.Utilice Herramientas &mdash. Buscar por identificador de recurso para buscar en todos
los archivos abiertos el identificador de recurso que especifique.
:p.:hp2.Mostrar IDs de recurso:ehp2.
:p.Utilice las propiedades de los temas (pulse un tema con el segundo bot¢n del rat¢n
y elija ÆPropiedadesØ) para ver quÇ identificadores de recurso est†n asociados
a un tema.

:h2 res=19 id='TopicNames'.Nombres de los temas
:p.:hp2.Nombres de los temas:ehp2.
:p.Igual que los IDs de recurso, los programadores pueden usar los nombres de los
temas para enlazar temas de ayuda desde sus aplicaciones, utilizando el mensaje
HM_DISPLAY_HELP con HM_PANELNAME como segundo par†metro.
:p.Estos no se utilizan tan a menudo.
:p.NewView puede encontrar un nombre de tema concreto utilizando Herramientas &mdash.
Buscar temas por nombre.
:euserdoc.
