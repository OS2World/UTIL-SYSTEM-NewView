.* Generated by VyperHelp on 06/06/04.
:userdoc.
:docprof toc=123456.
:title.Guida di NewView
.* Subject: Help for NewView
.* Version:
.* Copyright: Copyright 2004 Aaron Lawrence
.* Author: Aaron Lawrence
:h1 res=30000 id='Introduction'.
Introduzione
:i1 id=30001.support
:p.
:artwork runin name='images\NewView.bmp'.
:hp2.Benvenuti in NewView&xclm.:ehp2.
:p.
:p.NewView � un programma per la lettura dei file di aiuto di OS&slash.2 &lpar.o eComStation&rpar. &per.
:p.:link reftype=hd refid='Support'.Supporto e Licenza:elink.
:p.:link reftype=hd refid='Using'.Uso di NewView:elink.
:p.:hp1.Per impedire il caricamento di questo file al lancio di NewView senza specificare un file di aiuto&comma. consultare Strumenti &endash. Opzioni &endash. Generale&per.:ehp1.
:p.:hp2.Storia:ehp2.
:p.NewView sostituisce il programma originale di IBM fornito con OS&slash.2&per.
:p.Esso migliora molti aspetti di View&comma. con un'interfaccia moderna&comma. di facile uso&comma. pi� opzioni&comma. e funzioni che View semplicemente non ha&per.
:p.
:h2 res=1 id='Support'.
Supporto e Licenza
:i1 id=30002.Licenza
:i2 refid=30001.Supporto e Licenza
:i2 refid=30001.Introduzione
:i1 id=30003.Bug
:i1 id=30004.Codice sorgente
:p.:hp2.Supporto e Licenza:ehp2.
:p.
:p.NewView � Copyright 1999&endash.2003 Aaron Lawrence&per. Esso � licenziato inoltre sotto
la GNU Public License&comma. il che significa che l'utente ha il diritto di ottenerne il codice sorgente&per.
:p.Leggere il file Readme&per.txt per ulteriori dettagli tecnici&per.
:p.Consultare il file Changes&per.txt per l'elenco storico dei cambi fatti a NewView&per.
:p.Se ritenete NewView utile&comma. vi prego di mandarmi un'email e&slash.o fare una
donazione per supportare ulteriori sviluppi&per. E&apos. bello sentire commenti&xclm.
:p.o Suggerimenti&comma. complimenti o rapporti su bug &lpar.ma vi prego, siate gentili
&endash. in fin dei conti, � un programma gratuito &colon.&rpar. Spedite a
aaronl&atsign.consultant&per.com
.br
o Traducete NewView nella vostra lingua&per.
.br
o Blackstar gift certificate
http&colon.&slash.&slash.www&per.blackstar&per.co&per.uk
.br
o Voucher per Amazon&per.com http&colon.&slash.&slash.www&per.amazon&per.com
.br
o Una donazione tramite PayPal http&colon.&slash.&slash.www&per.paypal&per.com
:p.&per.&per.&per. o altro&comma. davvero&xclm. Fintanto che non mi costa molto per
ottenerlo&comma. ogni contributo � gradito&per.
&dollar. 5 tramite Paypal&comma. Moneybookers&per.com&comma. vaglia&comma.
etc&per. Attenzione&comma. molti di questi servizi sono poco costosi per l'acquirente&slash.donatore&comma. ma piuttosto dispendiosi per il "venditore" &endash.
centinaia di dollari US per anno non sono cifre rare&per.
:p.:hp2.Riportare bug:ehp2.
:p.Se dovete riportare un crash o un altro problema&comma. siate pi� specifici possibile
a riguardo dei file usati&comma. cosa stavate facendo&comma.
etc&per. Se disponibile&comma. vi PREGO di includere il file newview&per.log&per. Esso
sar� posto
.br
 &endash. nella stessa directory di NewView
.br
 &endash. nella cartella dichiarata nella variabile d'ambiente LOGFILES &lpar.tipico di eCS
1&per.1&plus.&rpar.
.br
Se si tratta di un particolare file di aiuto&comma. vi prego di spedirmelo&comma.
a meno che non siano file lunghi pi� di 1MB&per.
:p.La maggior parte delle informazioni che seguono � contenuta nel file newview&per.log&comma. ma
sarebbe d'aiuto includerla per le verifiche&colon.
:p.o Versione di NewView &lpar.Aiuto &endash. Informazioni sul prodotto&rpar.
:p.o Nome dei file di aiuto&per.
:p.Un'immagine catturata dallo schermo potrebbe essere utile&comma. se il problema � una
visualizzazione scorretta o corrotta&per.
:p.:hp2.Perch� il mio file di aiuto non funziona correttamente?:ehp2.
:p.Alcune delle caratteristiche meno usate dell'originale programma View non sono state
implementate&per. Ci� perch� non ci sono ancora riuscito&comma. o solo perch� non
ne valeva la pena&per. Tra gli esempi ci sono i metafile&comma. i sinonimi in indice&comma.
l'intera API di controllo dell'applicazione&comma.eccetera&per.
:p.Sfortunatamente&comma. pare che almeno uno sviluppatore abbia usato ognuna di
queste funzioni&comma. per cui ci sar� un file ogni tanto che non viene caricato o che non
viene letto correttamente&per.
:h1 res=2 id='Using'.
Uso di NewView
:p.:hp2.Uso di NewView:ehp2.
:p.Dopo aver :link reftype=hd refid='AprireFile'.aperto un
file:elink.&comma. � possibile leggerlo in vari modi&per.
:p.Se ne pu� leggere il :link reftype=hd refid='contents'.sommario:elink.&comma.
usare l':link reftype=hd refid='Index'.indice alfabetico:elink.&comma.
oppure :link reftype=hd refid='search'.cercare argomenti:elink.&per.
:p.Per leggere semplicemente il file di aiuto come un libro in carta&comma. si usano i tasti "Precedente"
:artwork runin name='images\previous.bmp'.
 e "Successivo"
:artwork runin name='images\next.bmp'.
 per scorrere tutte le voci&per.
:p.Il file di aiuto pu� essere letto come una serie di pagine web&comma. con i tasti "Indietro"
:artwork runin name='images\back.bmp'.
 e "Avanti"
:artwork runin name='images\forward.bmp'.
 per tornare alle pagine precedenti o ripetere i passi fatti&per.
:p.I colori e alcuni comportamenti di NewView possono venire modificati dal menu Strumenti
&endash. Opzioni&per.
:p.Inoltre � possibile :link reftype=hd refid='notes'.annotare:elink. o :link
reftype=hd refid='bookmarks'.aggiungere un segnalibro:elink. su argomenti&per.
:h1 res=3 id='AprireFile'.
Aprire file
:i1 id=30005.aprire
:p.:hp2.Aprire file di aiuto:ehp2.
:p.
:p.Per aprire un file di aiuto (di guida)&comma. si pu� usare uno dei metodi seguenti&colon.
:p.&endash. Doppio&endash.click su un':link reftype=hd refid='HelpIcons'.icona di file aiuto:elink.
gi� preparata
:p.&endash. Digitare "view :hp1.nome file:ehp1." da una :link reftype=hd
refid='CommandLine'.finestra comandi:elink.
:p.&endash. Click singolo sul tasto Apri
:artwork runin name='images\open.bmp'.
 dall'interno di NewView
:p.&endash. Ricaricare un file aiuto visto di recente dal menu "File"
:p.&endash. Trascinare un file aiuto dalla scrivania sull'icona di NewView
:p.Dopo che il file � stato caricato&comma. viene presentato il suo :link reftype=hd
refid='contents'.sommario:elink. e il primo argomento&per.
:p.:hp5.Nota:ehp5.&colon. qui supponiamo che NewView sia stato installato in sostituzione del
programma originale View&per. In caso contrario, le icone di aiuto esistenti e la linea di comando potrebbero avere effetti diversi&per.
:p.:hp2.Caricare insieme pi� file:ehp2.
:p.NewView pu� caricare pi� file in un solo momento&comma. presentandoli come se fossero
un solo libro&comma. e leggere le variabili d'ambiente per i nomi di file&per.
:p.Per esempio&comma. con la documentazione dell'OS&slash.2 Developer&apos.s Toolkit&colon.

:p.
  NewView cpref
:p.
.br

carica la "Control Program Guide and Reference"&per. CPREF � una variabile d'ambiente
impostata in config&per.sys&comma. consistente di "CP1&plus.CP2&plus.CP3"
che dice a NewView &lpar.o a View&rpar. di caricare i file guida CP1&comma. CP2
e CP3&per. I file vengono cercati nel percorso specificato da due :link
reftype=hd refid='L_EnvironmentVariables'.variabili d'ambiente:elink.&per.&asterisk.
:p.I file vengono tutti caricati ed effettivamente attaccati l'uno all'altro&per.
:p.Questa capacit� pu� essere utile per varie ragioni. Per esempio&comma. 4OS&slash.2 &lpar.un
sostituto di CMD&per.EXE&rpar. lo usa per aggiungere il suo aiuto specifico in cima all'aiuto originale
di CMD&per. Questo pu� essere fatto con tutti i file desiderati&per.
:p.Pi� file di aiuto possono essere caricati nella dialog Aprire file usando i tasti Ctrl o Shift per selezionare pi� file&per.
:p.Facendo click a un collegamento verso un diverso file di aiuto&comma. NewView carica tale file senza chiudere i file correntemente aperti&per.
:p.In qualsiasi momento&comma. � possibile vedere quali file sono aperti usando il menu File
&endash. Informazioni&per.
:p.:hp2.Caricare file aggiuntivi:ehp2.
:p.Si pu� attivare la checkbox "Mantieni aperto il file corrente" nella dialog di apertura dei file&comma. e NewView aprir� i file selezionati senza prima chiudere quelli attualmente aperti&per.
:p.:hp2.Drag and Drop:ehp2.
:p.Si possono trascinare file &per.INF o &per.HLP su NewView per aprirli&per.
Mantenendo premuto il tasto Shift&comma. i file saranno aperti senza chiudere i file
gi� aperti&per.
:p.I file possono essere lasciati cadere sulla finestra principale&comma. oppure sulle finestre
Sommario o Indice&comma. o sulla finestra di un argomento visibile&per.
:p.:hp5.Nota:ehp5.
:p.Alcuni collegamenti che portano ad altri file guida o aiuto&comma. funzioneranno solo se sono stati caricati i file opportuni&per.
:h2 res=17 id='HelpIcons'.
Icone aiuto
:p.:hp2.Icone aiuto:ehp2.
:p.Le icone aiuto sulla scrivania sono di solito oggetti programma in cui il nome del programma � "view&per.exe" e con parametro dato dal nome dei file di aiuto&per.
:p.Alcuni programmi creano queste icone al momento dell'installazione&per.
:p.Queste icone possono venire create direttamente tramite la maschera dei programmi&per.
Consultate l'aiuto sulla scrivania per informazioni&per.
:p.Se si creano le icone trascinando i file di aiuto sulla scrivania&comma. non � possibile dar loro un titolo significativo&comma. dato che ci� cambierebbe il nome del file e questo impedirebbe
a un programma di trovare il proprio file di aiuto o guida&per.
Pertanto, gli oggetti programma sono il metodo migliore raccomandato per creare icone di aiuto&per.
:h1 res=200 id='NavigationPanel'.
Le linguette del pannello di navigazione
:p.:hp2.Le linguette del pannello di navigazione:ehp2.
:p.Il pannello sulla sinistra contiene varie linguette per navigare nel file di aiuto in vari modi&per.
:p.:link reftype=hd refid='contents'.Sommario:elink.
:p.:link reftype=hd refid='Index'.Indice:elink.
:p.:link reftype=hd refid='search'.Ricerca:elink.
:p.:link reftype=hd refid='notes'.Note:elink.
:p.Questo pannello pu� essere escluso per avere pi� spazio&comma. selezionando Visualizza
&endash. Mostra pannello di sinistra dal menu&comma. o premendo Alt&plus.P&per. La stessa cosa
si fa per riaprirlo&per.
:p.Il pannello pu� essere istruito a non comparire quando un file viene aperto in
Strumenti &endash. Opzioni &endash. Generale&per.
:p.:hp2.Nota&colon. :ehp2. Molti programmi scelgono di mostrare il sommario quando aprono il file
di aiuto&semi. in tal caso&comma. il pannello viene mostrato automaticamente&comma. a dispetto di questa impostazione&per.
:h2 res=4 id='contents'.
Sommario
:i1 id=30006.sommario
:p.:hp2.Sommario:ehp2.
:p.
:p.La maggior parte dei file di aiuto ha un indice dei contenuti che mostra le voci di aiuto presenti nel file&comma. in forma gerarchica&per. Solitamente � la prima vista ottenuta
aprendo un file&per.
:p.I rami dell'albero possono essere espansi o riuniti premendo i bottoni &plus.
&comma. o usando la barra spazio&per.
:p.Per vedere una voce dall'indice&comma. basta selezionarla&per. Ci si pu� inoltre
muovere nell'indice usando i tasti cursore&per.
:p.Per muoversi attraverso &asterisk.tutte&asterisk. le voci nell'albero&comma. in
ordine&comma. si possono usare i tasti Ctrl &plus. Su e Ctrl &plus. Gi�&comma. oppure i
tasti "Precedente"
:artwork runin name='images\previous.bmp'.
 e "Successivo"
:artwork runin name='images\next.bmp'.
&per. Questa � una maniera per trattare il file come un comune libro&comma. leggendo pagina
per pagina&per.
:p.L'intero indice dei contenuti pu� essere letto usando l'opzione "Espandi" nel menu "Visualizza"&per. Ci� espande tutti i rami dell'indice dei contenuti, cos� da poterlo leggere completamente&per. Comunque&comma. � di solito pi� semplice usare la funzione
:link reftype=hd refid='search'.Ricerca:elink. o l':link reftype=hd
refid='Index'.indice:elink. a questo scopo&per.
:h2 res=5 id='Index'.
Indice
:p.:hp2.L'indice:ehp2.
:p.
:p.La linguetta Indice contiene un elenco alfabetico delle voci o delle parole chiave nel file
di aiuto&per. Battendo i primi caratteri della parola che si sta cercando, � possibile
trovare l'argomento stesso rapidamente&per. NewView salta automaticamente alla prima
voce corrispondente nell'indice&per. Per vedere la voce evidenziata basta premere
Invio&per.
:p.:hp5.Note:ehp5.
:p.I file di aiuto potrebbero contenere o meno un indice "ufficiale"&per. L'indice viene creato a
mano dall'autore&comma. per cui &lpar.per l'originale View&rpar. la sua utilit�
dipende strettamente dal lavoro fatto dall'autore sull'indice stesso&per.
Potrebbe anche non esservi un indice&per.
:p.Comunque&comma. un indice utile pu� essere fornito semplicemente elencando ogni voce
alfabeticamente&comma. cosa fatta da NewView&per. NewView inoltre
fonde l'indice originale &lpar.se presente&rpar. con l'elenco delle voci&per.
:p.Se per qualche motivo la cosa non piace&comma. � possibile eliminare questa funzione in
Strumenti &endash. Opzioni &endash. Generale&per.
:h2 res=6 id='search'.
Ricerca
:p.:hp2.Ricerca:ehp2.
:p.
:p.La funzione ricerca permette di trovare le informazioni rapidamente&comma. se non si sa dove
trovarle&per. Andando sulla linguetta Ricerca&comma. basta scrivere parole correlate all'argomento
e fare click sul tasto Ricerca&per.
:p.Verr� presentato un elenco di tutte le voci contenenti quella parola&comma. o parole
simili&comma. con le voci pi� simili in cima&per. La voce pi� simile viene
mostrata automaticamente&per.
:p.Le parole che corrispondono alla ricerca vengono evidenziate nel testo&per.
:p.:hp2.Ricerca globale:ehp2.
:p.Si possono cercare tutti i file di aiuto sul sistema usando la :link reftype=hd
refid='GlobalSearch'.ricerca globale:elink. in Strumenti &endash. Cerca in tutti i file di
aiuto&per.
:p.:hp2.Ricerca per frasi:ehp2.
:p.Per cercare una frase di pi� di una parola&comma. basta porre la frase fra
doppie virgolette&comma. per esempio "os&slash.2 warp"&per.
:p.:hp2.Caratteristiche di corrispondenza:ehp2.
:p.NewView permette un controllo pi� raffinato nella ricerca&per.
:p.&plus. indica una parola che :hp2.deve:ehp2. corrispondere esattamente
:p.&endash. indica una parola che :hp2.non:ehp2. deve corrispondere
:p.NewView accetta sempre corrispondenze parziali&per. Ossia&comma. cercando la parola
"fin" NewView trover� anche "finestra" e "finale"&per. Comunque&comma. l'argomento con
la migliore corrispondenza verr� considerato di maggior valore&per.
:p.:hp2.Come NewView considera i risultati:ehp2.
:p.NewView classifica le voci trovate in vari modi&colon.
:p.&endash. corrispondenza pi� vicina a una parola
:p.&endash. numero di parole corrispondenti in un argomento
:p.&endash. corrispondenze nel titolo
:p.&endash. corrispondenze con una voce di indice
:h2 res=7 id='notes'.
Note
:p.:hp2.Aggiungere e usare le note:ehp2.
:p.
:p.NewView permette di aggiungere note &lpar.annotazioni&rpar. ai file di aiuto&per.
:p.Per aggiungere una nota&comma. basta fare click nel punto da annotare e premere il bottone
"Note"
:artwork runin name='images\note.bmp'.
&comma. quindi inserire il testo voluto e premere Invio&per. Il testo verr� inserito nella voce di
aiuto con un colore diverso &lpar.verde � il colore predefinito&semi. � possibile cambiare il
colore in Strumenti &endash. Opzioni &endash. Colori&rpar.&per.
:p.Per modificare o cancellare una nota&comma. basta fare click sul testo della nota&semi. � possibile
ora modificarne il testo&comma. o cancellarlo con il bottone Cancellare&per.
:p.Le note apposte nel file di aiuto possono inoltre essere esaminate, selezionando la linguetta
Note&semi. qui � possibile aggiungere&comma. modificare e cancellare note&comma.
ma anche saltare alle voci contenenti le note&per.
:p.
:p.Le note vengono salvate in un file con estensione &per.nte&comma. nella stessa cartella
del file di aiuto che accompagnano&per.
:p.Nota &endash. se un file di aiuto viene modificato &lpar.per esempio, nell'aggiornamento di un
programma&rpar. le note non appariranno pi� nella posizione corretta&semi.
� comunque possibile leggerle dalla linguetta Note&per.
:h1 res=18 id='GlobalSearch'.
Ricerca globale
:p.:hp2.Ricerca globale:ehp2.
:p.Usando il bottone "Cerca tutto" :artwork runin name='images\search.bmp'.
&comma. � possibile effettuare una ricerca in tutti i file di
aiuto nel sistema&semi..
si pu� usare anche il menu Strumenti &endash. Cerca in tutti i file di aiuto&comma. oppure
si pu� premere Ctrl&plus.S&per.
:p.Questa ricerca funziona in modo simile alla :link reftype=hd refid='search'.ricerca in un
file:elink.&comma. ma informa anche sul file di aiuto o guida in cui sono stati trovati
i risultati&per.
:p.:hp2.Dove NewView effettua le ricerche:ehp2.
:p.Normalmente la ricerca viene effettuata nei file di aiuto nei percorsi specifici&comma. definiti
nelle :link reftype=hd refid='L_EnvironmentVariables'.variabili d'ambiente:elink. BOOKSHELF e HELP&per. Le cartelle
presenti in tali percorsi possono essere viste premendo il bottone "Visualizza" a fianco dell'opzione "Percorsi di aiuto"&per.
:p.Si pu� effettuare una ricerca anche sull'intero sistema&per. Normalmente questo porta a una
ricerca su tutti i dischi rigidi&per. Le unit� possono essere aggiunte o rimosse dalla ricerca tramite
il bottone "Seleziona" a fianco dell'opzione "Unit�"&per.
:p.La ricerca totale pu� trovare tutti i file di aiuto&comma. ma pu� essere molto pi� lenta della ricerca nei percorsi specifici&per.
:h1 res=8 id='bookmarks'.
Segnalibri
:p.:hp2.Segnalibri:ehp2.
:p.
:p.NewView permette di marcare voci particolari nel file di aiuto&per. Basta premere
il bottone "Segnalibri"
:artwork runin name='images\bookmark.bmp'.
 per aggiungere la voce corrente come segnalibro&per.
:p.Per saltare a un segnalibro&comma. basta andare al menu "Segnalibri"&comma. e fare click
sul segnalibro desiderato&per.
:p.I segnalibri possono essere esaminati e cancellati usando la voce "Modifica&per.&per.&per." nel
menu "Segnalibri"&per. La finestra pu� rimanere aperta durante la lettura&comma. quindi �
possibile guardare rapidamente i segnalibri&per.
:p.
:p.:hp5.Note:ehp5.
:p.I segnalibri di NewView ricordano tutte le finestre di argomenti aperte&comma. se pi� di una finestra � aperta&per.
:p.I segnalibri vengono salvati in un file con estensione &per.bmk&comma. nella stessa cartella del file di aiuto cui si riferiscono&per.
:h1 res=100 id='InternetLinks'.
Collegamenti Internet
:p.:hp2.Collegamenti Internet:ehp2.
:p.Selezionando col mouse un URL come
http&colon.&slash.&slash.www&per.google&per.com&comma. NewView lancer� il browser
web predefinito&per.
:p.Il browser predefinito � specificato nelle impostazioni del sistema operativo&comma. non da NewView&per.
Per configurare il browser&comma. si pu� aprire un oggetto URL sulla scrivania&comma.
modificare il percorso al browser nella sezione :hp2.Browser:ehp2. tab&comma. e quindi premere
il bottone Imposta predefinito&per.
:p.Un browser pu� anche rendersi automaticamente predefinito&comma. al tempo di
installazione oppure nelle sue preferenze&per.
:p.Collegamenti email&comma. newsgroup ed FTP vengono passati al browser&per.
Attualmente&comma. non � possibile scegliere un altro programma per tali collegamenti&per.
:p.:hp2.Nota per gli autori dei file di aiuto:ehp2.
:p.L'originale programma View non comprendeva collegamenti URL o email&comma. per cui l'unica
via per implementarli era un collegamento a programma&comma. per esempio&comma.
"netscape&per.exe" con i parametri corretti&per.
:p.NewView traduce i collegamenti a "netscape"&comma. "explore" o "mozilla" in collegamenti
al browser predefinito&per.
:p.Esso inoltre rileva automaticamente le URL nelle forme&colon.
:p. http&colon.&slash.&slash.  https&colon.&slash.&slash.
 ftp&colon.&slash.&slash.
:p. mailto&colon.  news&colon.
:p.Tutto ci� che somiglia a un URL viene rilevato&comma. anche senza il prefisso di
protocollo&colon.
:p. www&per.a&per.b &endash. browser
:p. ftp&per.a&per.b &endash. ftp
:p. a&atsign.b&per.c &endash. email
:p.dove a&comma. b e c sono stringhe alfanumeriche&per.
:p.Non � necessario fare alcunch� perch� NewView li riconosca&per.
:h1 res=9 id='CommandLine'.
Parametri della linea di comando
:p.:hp2.Parametri della linea di comando:ehp2.
:p.Lanciando NewView dalla linea di comando, possono essere forniti vari
parametri&per. Nessuno di essi � indispensabile&per.
:p.
:p.:hp2.NewView &lbracket.opzioni&rbracket. &lbracket.&lt.filename&gt.
&lbracket.&lt.cerca testo&gt.&rbracket.&rbracket.:ehp2.
:p.Se NewView � installato come sostituto di view&comma. il comando � lo stesso&comma.
con view al posto di newview&per.
:p.:link reftype=hd refid='CommandLineExamples'.Esempi:elink.
:p.:hp2.&lt.filename&gt.:ehp2.
:p.Il file da far leggere a NewView&per. Possono essere caricati pi� file per volta, usando la
notazione filename1&plus.filename2&comma. etc&per.
:p.Se non viene specificato un percorso&comma. i file vengono cercati nei percorsi specificati da
:link reftype=hd refid='L_EnvironmentVariables'.BOOKSHELF e HELP:elink.&per.
:p.:hp2.&lt.cerca testo&gt.:ehp2.
:p.Ricerca titoli di argomenti e voci di indice per il testo specificato&per.
:hp2.Non:ehp2. equivale a una ricerca normale&comma. per compatibilit� con
l'originale View&per. Per una ricerca normale, si usi l'opzione &slash.s &lpar.vedi
sotto&rpar.&per. Per dettagli&comma. vedere :link reftype=hd
refid='CommandLineTopicSearch'.Ricerca di argomenti dalla linea di comando:elink.&per.
:p.:hp2.Opzioni:ehp2.
:p.:hp2.&slash.s&colon.&lt.ricerca di testo&gt.:ehp2.
:p.Dopo aver aperto il file&comma. esegue una :link reftype=hd
refid='search'.ricerca:elink. del testo dato&per.
:p.:hp2.&slash.g&colon.&lt.ricerca di testo&gt.:ehp2.
:p.Esegue una :link reftype=hd refid='GlobalSearch'.ricerca globale:elink. per il
testo dato&comma. su tutti i file di aiuto del sistema&per.
:p.:hp2.&slash.?:ehp2. o :hp2.&slash.h:ehp2. o :hp2.&slash.help:ehp2.
:p.Mostra l'aiuto per la linea di comando&per.
:p.Vedi anche&colon. :link reftype=hd refid='AdvancedParameters'.Parametri
avanzati:elink.
:h2 res=13 id='CommandLineTopicSearch'.
Ricerca di argomenti dalla linea di comando
:p.:hp2.Ricerca di argomenti dalla linea di comando:ehp2.
:p. view &lt.nome file&gt. &lt.voce&gt.
:p.Il parametro di ricerca specificato sulla linea di comando replica il comportamento dell'originale
View&per.
:p.Non viene cercato il testo all'interno delle voci&comma. ma solo titoli e voci di indice&per.
Questo � meno utile all'utente&comma. ma � usato da alcuni programmi per riferire le
voci dell'aiuto con risultati predicibili&per.
:p.Possono venire usate pi� parole&per.
:p.La ricerca eseguita �&colon.
:p.&endash. il titolo dell'argomento inizia per il testo cercato
:p.&endash. la voce di indice inizia per il testo cercato
:p.&endash. il titolo dell'argomento contiene il testo cercato
:p.&endash. la voce di indice contiene il testo cercato&per.
:p.Sviluppatori: assicuratevi che il documento atteso venga trovato se si usa questa tecnica
per identificare le voci nell'uso di View o di NewView&per.
:h2 res=14 id='AdvancedParameters'.
Parametri avanzati
:p.:hp2.Parametri avanzati:ehp2.
:p.I parametri seguenti sono considerati soprattutto per gli sviluppatori di
software&comma. ma possono essere usati da chiunque&per.
:p.:hp2.&slash.lang&colon.&lt.lingua&gt.:ehp2.
:p.Carica la lingua specificata&per. Ha precedenza su quella scelta in base alla variabile
d'ambiente LANG&per. Per esempio&comma. &slash.lang&colon.en carica
la lingua Inglese&per. Per maggiori informazioni, consultate il file readme&per.txt&per.
:p.:hp2.&slash.pos&colon.&lt.sin&gt.&comma.&lt.des&gt.&comma.&lt.amp&gt.&comma.&lt.alt&gt.:ehp2.
:p.Imposta la posizione e le dimensioni della finestra principale del programma&per. I valori devono
essere tutti presenti&per. Ponendo :hp2.P:ehp2. dopo un numero, si indica che si sta specificando una
percentuale&per. Per esempio&colon.
:p.newview &slash.pos&colon.10P&comma.10P&comma.80P&comma.80P
:p.centra la finestra e la dimensiona all'80&percent. delle dimensioni dello schermo&per.
:p.:hp2.&slash.title&colon.&lt.titolo&gt.:ehp2.
:p.Imposta il titolo della finestra di NewView al testo specificato&comma. a dispetto di qualsiasi
cosa appaia nel file di aiuto&per. Il testo "Aiuto &endash.  " sar� presente comunque
davanti al testo specificato&comma. a meno che questo non sia
"help"&comma. nel qual caso il titolo sar� semplicemente "Help"&per. Ci� per assicurarsi
che le finestre di aiuto siano sempre evidenti come tali nell'elenco delle finestre&per.
:p.Volendo specificare parole multiple&comma. basta chiudere l'opzione tra virgolette&per.
Per esempio&colon. newview cmdref "&slash.title&colon.Aiuto della linea di comando"&per.
:h2 res=15 id='CommandLineExamples'.
Esempi della linea di comando
:p.:hp2.Esempi della linea di comando:ehp2.
.br
:p.Negli esempi che seguono, supponiamo che newview sia installato come sostituto&comma.
per cui "view" � effettivamente equivalente a "newview"&per.
:p.:hp2.view cmdref:ehp2.
:p.Apre il file cmdref&per.inf &lpar.Riferimento ai comandi di OS&slash.2&rpar. dal percorso dei
file di aiuto&per.
:p.:hp2.view cmdref&plus.os2ug:ehp2.
:p.Apre i due file&comma. cmdref&per.inf e os2ug&per.inf &lpar.Guida utente di OS&slash.2&rpar.
&comma. nella stessa finestra&per.
:p.Il sommario di os2ug&per.inf viene aggiunto alla fine del sommario di
cmdref&per.inf&per. Gli indici vengono combinati alfabeticamente&per.
:p.:hp2.view c&colon.&bsl.os2&bsl.book&bsl.os2ug&per.inf:ehp2.
:p.Apre il file os2ug&per.inf nel percorso c&colon.&bsl.os2&bsl.book&per.
:p.:hp2.view cmdref dir:ehp2.
:p.Apre il file cmdref &lpar.Riferimento ai comandi di OS&slash.2&rpar. e cerca fra titoli e indice
la parola "dir"&per. Mostrer� la pagina di aiuto per il comando DIR&per.
:p.:hp2.view &slash.s&colon.desktop os2ug:ehp2.
:p.Apre il file os2ug&per.inf e cerca la parola "desktop"&per. Viene mostrato il risultato
pi� simile&per.
:p.:hp2.view &slash.g&colon.permissions:ehp2.
:p.Ricerca tutti i file di aiuto per trovare la parola "permissions"&per.
:p.:hp2.set myhelp&eq.cmdref&plus.os2ug&plus.rexx:ehp2.
:p.:hp2.view myhelp:ehp2.
:p.La prima riga imposta una variabile d'ambiente MYHELP per contenere i nomi di tre file
di aiuto&per. La seconda riga apre i file&per.
:h1 res=10 id='KeyboardShortcuts'.
Scorciatoie da tastiera
:p.:hp2.Scorciatoie da tastiera:ehp2.
:p.La maggior parte delle scorciatoie da tastiera � presente nei menu&comma. ma non tutte&per.
Le scorciatoie addizionali sono&colon.
:p.:hp2.Alt&plus.F4:ehp2. Esce
:p.:hp2.Ctrl&plus.C:ehp2. Copia il testo selezionato negli Appunti
:p.:hp2.F7:ehp2. Indietro
:p.:hp2.F8:ehp2. Avanti
:p.:hp2.Ctrl&plus.Left:ehp2. Indietro
:p.:hp2.F11:ehp2. Precedente nel sommario
:p.:hp2.F12:ehp2. Successiva nel sommario
:p.
:p.:hp2.Scorciatoie visibili nei menu:ehp2.
:p.:hp2.Ctrl&plus.O:ehp2. Apre un file
:p.:hp2.Ctrl&plus.E:ehp2. Apre un file dai percorsi di aiuto
:p.:hp2.Ctrl&plus.N:ehp2. Apre una nuova finestra
:p.:hp2.Ctrl&plus.P:ehp2. Stampa l'argomento
:p.:hp2.F3:ehp2. Esce
:p.
:p.
:p.:hp2.Ctrl&plus.A:ehp2. Seleziona tutto il testo nell'argomento
:p.:hp2.Ctrl&plus.Ins:ehp2. Copia il testo selezionato negli Appunti
:p.
:p.:hp2.Ctrl&plus.F:ehp2. Trova testo nell'argomento corrente
:p.:hp2.Ctrl&plus.G:ehp2. Ripete l'ultima ricerca di testo
:p.
:p.:hp2.Ctrl&plus.S:ehp2. Apre la finestra di ricerca globale
:p.
:p.:hp2.Alt&plus.C:ehp2. Va alla linguetta Sommario
:p.:hp2.Alt&plus.I:ehp2. Va alla linguetta Indice
:p.:hp2.Alt&plus.S:ehp2. Va alla linguetta Ricerca
:p.:hp2.Alt&plus.N:ehp2. Va alla linguetta Note
:p.:hp2.Alt&plus.P:ehp2. Apre o chiude il pannello di sinistra
:p.:hp2.F5:ehp2. Espande l'intero sommario
:p.:hp2.F6:ehp2. Comprime l'intero sommario
:p.
:p.:hp2.Esc:ehp2. Indietro
:p.:hp2.Ctrl&plus.Right:ehp2. Avanti
:p.:hp2.Ctrl&plus.Up:ehp2. Argomento precedente nel sommario
:p.:hp2.Ctrl&plus.Down:ehp2. Argomento successiva nel sommario
:p.
:p.:hp2.Ctrl&plus.D:ehp2. Modifica i segnalibri
:p.:hp2.Ctrl&plus.B:ehp2. Marca l'argomento corrente come segnalibro
:p.
:p.:hp2.Ctrl&plus.M:ehp2. Aggiunge una nota alla posizione del cursore
:p.
:p.:hp2.F1:ehp2. Apre l'aiuto di NewView
:h1 res=11 id='L_EnvironmentVariables'.
Variabili d'ambiente
:p.:hp2.Variabili d'ambiente:ehp2.
:p.
:p.Le variabili :hp2.BOOKSHELF:ehp2. ed :hp2.HELP:ehp2. definiscono percorsi
&lpar.elenchi di cartelle&rpar. per la ricerca dei file di aiuto&per.
NewView usa ambo i percorsi senza distinzione&per.
:p.Tali percorsi vengono scorsi quando l'utente&colon.
:p.&endash. specifica un file di aiuto senza percorso&comma. da linea di comando
:p.&endash. usa il comando di menu File &endash. Apri speciale&per.&per.&per.
:p.&endash. fa una :link reftype=hd refid='GlobalSearch'.ricerca globale:elink.
:p.Si possono aggiungere permanentemente cartelle di file di aiuto alle variabili :hp2.HELP:ehp2. o
:hp2.BOOKSHELF:ehp2. modificando il file CONFIG&per.SYS&per. Per permettere al vecchio view di
trovare i file, � bene aggiungere i percorsi a entrambe le variabili&per.
:p.:hp2.Altre variabili d'ambiente:ehp2.
:p.:hp2.LANG:ehp2. viene esaminata per decidere il linguaggio che NewView user�
come predefinito&per. &lpar.Viene ignorata specificando il
 :link reftype=hd refid='AdvancedParameters'.parametro da linea di
comando:elink. &slash.lang&per.&rpar. Si legga il file readme&per.txt per maggiori informazioni
sui linguaggi&per.
:p.La cartella definita in :hp2.LOGFILES:ehp2. viene usata per registrare crash o
altre informazioni&per.
:p.La sottocartella "lang" nella cartella definita da :hp2.OSDIR:ehp2. viene esaminata per i
file lingua all'avvio&per.
:p.Il percorso :hp2.ULSPATH:ehp2. viene esaminato anch'esso per i file lingua&per.
:h1 res=24 id='ForAuthors'.
Per autori e sviluppatori
:p.:hp2.Per autori e sviluppatori:ehp2.
:p.
:p.Questa sezione contiene note per autori di documenti e sviluppatori
software&per.
:h2 res=12 id='WritingHelpFiles'.
Scrivere file di aiuto
:p.:hp2.Scrivere file di aiuto per OS&slash.2:ehp2.
:p.
:p.I file di aiuto di OS&slash.2 vengono prodotti usando il compilatore IPF&per. IPF vuol dire
Information Presentation Facility&per.
:p.Il compilatore IPF prende un file di testo scritto in un linguaggio che informa di cose come
intestazioni&comma. collegamenti&comma. testo e immagini&comma. e produce
un file &per.INF o &per.HLP&per.
:p.La strada ufficialer per ottenere il compilatore IPF &lpar.ipfc&per.exe&rpar. � di andare
all'OS&slash.2 Developers Toolkit&per. Questo � gratuito con eComStation
&lpar. http&colon.&slash.&slash.www&per.ecomstation&per.com &rpar.&per.
:p.Dato che il linguaggio per IPFC � noioso &lpar.per esempio&comma. la punteggiatura deve venire
inserita con parole chiave speciali&comma. come &amp.comma&per.&rpar. molte persone usano
altri strumenti oltre al compilatore IPF stesso&per.
:p.Io uso Vyperhelp
&lpar. http&colon.&slash.&slash.www&per.vyperhelp&per.com &rpar. dato che �
semplice e grafico&per. Esso inoltre esporta i file in formato Windows Help&comma. HTML e
altri&comma. anche se gira solo su OS&slash.2&per. Non gratuito&per.
:p.Altre opzioni popolari sono&colon.
:p.&endash. HyperText&slash.2 IPF Preprocessor
&lpar.http&colon.&slash.&slash.www&per.clanganke&per.de&slash.os2&slash.sw&slash.htext&slash.
 &rpar. &endash. preprocessa un linguaggio pi� semplice nel formato IPF, molto
complesso&per. Gratuito&per.
:p.&endash. HyperMake
&lpar. http&colon.&slash.&slash.www&per.hypermake&per.com &rpar.&per.
Simile&comma. ma pu� produrre file Windows Help e HTML&per.
:p.o Sibyl &lpar.con cui � stato creato NewView&rpar. contiene un preprocessore
IPF&per.
:p.&endash. IPFEditor da PCS
&lpar. http&colon.&slash.&slash.www&per.pcs&endash.soft&per.com&slash.productipfe212&per.htm
 &rpar.&per. Forse il pi� completo&comma. ma dal costo notevole&per.
Nota&colon. NewView non supporta tutte le funzionalit� di IPFE&xclm.
.br
In passato esistevano molte altre opzioni&per. Quelle elencate dovrebbero comunque essere
disponibili e in qualche modo supportate&per.
:h2 res=16 id='TopicResourceIDs'.
ID risorsa voce
:p.:hp2.ID risorsa voce:ehp2.
:p.Gli ID risorsa vengono usati dagli autori degli aiuti in linea per le applicazioni&comma. per
identificare le voci di aiuto&per. L'applicazione chiama lo Help Manager specificando un
ID risorsa&comma. direttamente tramite il messaggio HM&us.DISPLAY&us.HELP&comma.
o indirettamente tramite le tabelle di aiuto inserite nelle risorse&comma. gestite direttamente
da PM&per. L'ID risorsa viene immagazzinato in una tabella interna al file
aiuto&per.
:p.Per autori di documenti&comma. NewView offre la possibilit� di vedere e trovare gli
ID risorsa&per.
:p.:hp2.Trova argomento per ID risorsa:ehp2.
:p.Usate Strumenti &endash. Trova argomento per ID risorsa per cercare un ID risorsa specifico in tutti i
file aperti&per.
:p.:hp2.Mostrare gli ID risorsa:ehp2.
:p.Usate le propriet� delle voci &lpar.tasto destro del mouse &endash. Propriet�&rpar. per vedere
quale ID risorsa � associato a una voce di aiuto&per.
:h2 res=19 id='TopicNames'.
Nomi delle voci
:p.:hp2.Nomi delle voci:ehp2.
:p.
:p.Come gli ID risorsa&comma. i nomi delle voci possono essere usati dagli sviluppatori per collegarsi
a voci di aiuto dall'interno dell'applicazione&comma. usando il messaggio HM&us.DISPLAY&us.HELP
con due parametri&comma. HM&us.PANELNAME&per.
:p.Questi non sono usati molto spesso&per.
:p.NewView pu� trovare una particolare voce&comma. usando Strumenti &endash. Trova
nome voce&per.
:euserdoc.
