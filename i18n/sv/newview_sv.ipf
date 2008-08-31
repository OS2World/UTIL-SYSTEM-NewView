:userdoc.
:docprof toc=123456.
:title.NewView hj„lp

:h1 id=0 res=30000.Introduktion

:p.:artwork name='img0.bmp' runin. :hp2.V„lkommen till NewView&xclm.:ehp2.
:p.
:p.NewView „r ett program f”r att l„sa OS&slash.2 &lpar.eller eComStation&rpar. hj„lpfiler&per.
:p.:link reftype=hd refid=1.Support och Licens:elink.
:p.:link reftype=hd refid=2.Anv„nda NewView:elink.
:p.:hp1.F”r att hinda den h„r filen fr†n att upptr„da n„r du startar NewView utan att specificera en hj„lpfil&comma. se Verktyg
&endash. Alternativ &endash. fliken Allm„nt&per.:ehp1.
:p.:hp2.Historik:ehp2.
:p.NewView ers„tter IBM's originalprogram som skeppas med OS&slash.2&per.
:p.Den f”rb„ttrar m†nga aspekter hos View&comma. med ett modernt interface&comma.
enkel att anv„nda&comma. med fler alternativ&comma. och nya funktioner som View helt enkelt saknar&per.
:p.
:h2  id=1 res=1.Support och Licens

:p.:hp2.Support och Licens:ehp2.
:p.
:p.NewView „r Copyright 1999 f”r&endash.2006 Aaron Lawrence&per. Den „r ocks† licensierad under
GNU Public License&comma. vilket betyder att du har r„ttigheter att anv„nda
k„llkoden&per.
:p.Sedan 2006 underh†ller Ronald Brill produkten&per.
.br
NewView „r ett Netlabs projekt&per.
:p.Se filen Readme&per.txt f”r fler tekniska detaljer&per.
:p.Se filen Changes&per.txt f”r en historik ”ver „ndringar hos NewView&per.
:p.Om du finner NewView anv„ndbar&comma. var v„nlig och skicka ett e-mail
till mig&slash.eller g”r en donation f”r att st”dja ytterligare utveckling&per. Det „r trevligt att h”ra av dig&xclm.
:p.F”rslag&comma. komplimanger eller buggrapporter http&colon.//svn&per.netlabs&per.org
/newview
:p.™vers„tt NewView till ditt spr†k&per.
:p.En donation till Netlabs http&colon.//www&per.mensys&per.nl
.br

:p.:hp2.Rapportera buggar:ehp2.
:p.Om du beh”ver rapportera en krasch eller andra problem&comma. var s† specifik som m”jligt
om vilka filer som anv„ndes&comma. vad du h”ll p† med&comma.etc&per. Om den finns
tillg„nglig&comma. VAR VŽNLIG och inkludera newview&per.loggfilen&per. Loggfilen finns ANTINGEN i
.br
 &endash. i samma katalog som NewView
.br
 &endash. i en katalog som st„llts in av milj”variablen LOGFILES &lpar.typiskt eCS 1&per.1&plus.&rpar.
.br
Om det „r specifikt f”r en viss hj„lpfil&comma. skicka „ven den till mig&comma.
om den inte „r st”rre „n 1MB&rpar.&per.
:p.Det mesta av den f”ljande informationen finns i newview&per.loggfilen&comma. men
det „r v„rdefullt om du kan inkludera den f”r verifiering&colon.
:p.NewView version &lpar.Hj„lp &endash. Produktinformation&rpar.
:p.Filnamnen p† hj„lpfilerna&per.
:p.En sk„rmdump kan vara v„rdefull&comma. om problemet „r en felaktig eller
korrupt display&per.
.br

:p.:hp2.Varf”r fungerar inte min hj„lpfil korrekt?:ehp2.
:p.Vissa av de minst anv„nda funktionerna hos original View-programmet „r inte
implementerade&per. Detta beror antingen p† att jag inte kunnat kunnat komma runt problemen&comma.
eller p† att det helt enkelt inte „r v„rt den tid det skulle ta&per. Exempelvis inkludering av
metafiler&comma. indexsynonymer&comma. hela applikationskontroll API'n&comma. och s† vidare&per.
:p.Olyckligtvis&comma. verkar det som †tminstone en utvecklare har anv„nt varenda en av dessa
funktioner&comma. s† ibland kanske du kommer att hitta en fil som inte laddas eller
fungerar ordentligt&per.
:h1  id=2 res=2.Anv„nda NewView

:p.:hp2.Anv„nda NewView:ehp2.
:p.N„r du v„l har :link reftype=hd refid=3.”ppnat en fil:elink.&comma. kan du l„sa den p† olika s„tt&per.
:p.Du kan l„sa :link reftype=hd refid=6.inneh†llsf”rteckningen:elink.&comma.
anv„nda det :link reftype=hd refid=7.alfabetiska indexet:elink.&comma. eller :link reftype=hd refid=8.s”ka:elink.&per.
:p.F”r att p† enklaste s„tt l„sa filen som en pappersbok&comma. anv„nd "F”reg†ende" :artwork name='img1.bmp' runin.
och "N„sta" :artwork name='img2.bmp' runin. knapparna f”r att f”rflytta dig genom alla „mnen&per.
:p.Du kan ocks† anv„nda hj„lpfiler som webbsidor&comma. genom att anv„nda "Bak†t" :artwork name='img3.bmp' runin.
och "Fram†t" :artwork name='img4.bmp' runin. knapparna f”r att g† tillbaka dit du var f”re&comma. eller f”r
att †terskapa dina steg&per.
:p.F„rg och vissa beteenden hos NewView kan justeras fr†n Verktygsmenyn
&endash. Alternativ&per.
:p.Du kan ocks† g”ra :link reftype=hd refid=9.noteringar:elink. eller ett :link reftype=hd refid=11.bokm„rke:elink. f”r „mnen&per.
:h1  id=3 res=3.™ppna filer

:p.:hp2.™ppna hj„lpfiler:ehp2.
:p.
:p.F”r att ”ppna en hj„lpfil&comma. kan du anv„nda n†got av f”ljande&colon.
:p.&endash. Dubbel&endash.klicka p† :link reftype=hd refid=4.hj„lpikoner:elink. som redan finns
:p.&endash. Skriv "view :hp1.filnamn:ehp1." i en :link reftype=hd refid=13.'CommandLine'.kommandosession:elink.
:p.&endash. Klicka p† knappen ™ppna :artwork name='img5.bmp' runin. inom NewView
:p.&endash. Ladda om en nyligen visad fil fr†n "Arkiv" menyn
:p.&endash. Drag och sl„pp en hj„lpfil fr†n skrivbordet
:p.Direkt n„r filen „r laddad&comma. b”r du kunna se :link reftype=hd
refid=6.inneh†llsf”rteckningen:elink. och det f”rsta „mnet&per.
:p.:hp5.Notera&colon.:ehp5. Detta f”ruts„tter att du har installerat NewView som en ers„ttning f”r
originalet View&per. Om du inte gjort det, kan existerande hj„lpikoner och kommandoraden bete sig
annorlunda&per.
:p.:hp2.Ladda multipla filer p† en g†ng:ehp2.
:p.NewView kan ladda multipla filer samtidigt&comma. och presentera dom som om de vore en enda bok&comma. och l„sa milj”variablerna
f”r filnamnen&per.
:p.Till exempel&comma. med OS&slash.2 Developer&apos.s Toolkit dokumentationen&colon.
.br
  NewView cpref
.br
laddar "Control Program Guide and Reference"&per. CPREF „r en milj”variabel inst„lld
i config&per.sys&comma. best†ende av "CP1&plus.CP2&plus.CP3" vilken talar om f”r
NewView &lpar.eller View&rpar. att ladda hj„lpfilerna CP1&comma. CP2 och CP3&per. Filerna efters”ks i den s”kv„g som specificerats av tv† :link
reftype=hd refid=18.milj”variabler:elink.&per.&asterisk.
:p.Alla filer laddas och ser ut som en enda fil&per.
:p.Att ladda multipla filer p† detta s„tt kan vara v„rdefullt av olika
sk„l&per. Till exempel&comma.
4OS&slash.2 &lpar. CMD&per.EXE ers„ttaren&rpar. anv„nder detta f”r att ladda sin egen hj„lpfil
ovanp† originalets CMD hj„lp&per. Du kan g”ra det sj„lv med n†gra filer som du vill anv„nda&per.
:p.Du kan ladda multipla filer i Arkiv ™ppna hj„lpfil dialogen genom att anv„nda Ctrl eller Shift f”r
att v„lja multipla filer&per.
:p.N„r du klickar p† en l„nk till en annan hj„lpfil&comma. laddar NewView filen utan att st„nga
dina aktuella filer&per.
:p.N„r som helst&comma. kan du se vilka filer som „r ”ppnade genom att anv„nda Arkiv &endash.
Information&per.
:p.:hp2.Ladda ytterligare filer:ehp2.
:p.Du kan markera checkboxen "Beh†ll nuvarande fil ”ppen" i dialogen ™ppna fil&comma.
och NewView kommer att ”ppna de filer som du har valt utan att st„nga de f”r n„rvarande ”ppna
filerna&per.
:p.:hp2.Drag och Sl„pp:ehp2.
:p.Du kan dra och sl„ppa &per.INF eller &per.HLP filer p† NewView och de kommer att ”ppnas&per.
Om du h†ller ner Shift-tangenten&comma. kommer de att ”ppnas utan att st„nga de
aktuella filerna&per.
:p.Du kan sl„ppa filer p† n†gon av huvudinneh†llsytorna&comma. som Inneh†lls- eller
Indexf”nstren&comma. eller ett existerande „mnesf”nster&per.
:p.:hp2.Notera:ehp2. Vissa l„nkar som g†r mellan filer&comma. kommer endast att fungera om korrekt
upps„ttning filer „r laddade&per.
:h2  id=4 res=17.Hj„lpikoner

:p.:hp2.Hj„lpikoner:ehp2.
:p.Hj„lpikoner p† skrivbordet „r vanligtvis "programobjekt" med programnamnet inst„llt p†
"view&per.exe" och parametrar satta till namnet
p† hj„lpfilerna&per.
:p.Vissa program skapar dessa ikoner automatiskt vid installationstillf„llet&per.
:p.Du kan skapa dessa ikoner sj„lv genom att anv„nda skrivbordets program-mall&per. Se
hj„lpen f”r skrivbordet f”r mer information&per.
:p.Om du skapar ikoner genom att dra hj„lpfiler till skrivbordet&comma. kan du inte ge dom
en meningsfull rubrik&comma. beroende p† att det skulle f”r„ndra namnet p† filen&comma.
vilken kan f”rhindra program fr†n att hitta sina hj„lpfiler&per. D„rf”r „r programobjekt det
rekommenderade s„ttet att skapa hj„lpikoner&per.
:h1  id=5 res=200.Navigeringspanel flikar

:p.:hp2.Navigationspanel flikar:ehp2.
:p.Den v„nstra panelen inneh†ller ett flertal flikar f”r att kunna flytta runt i den
aktuella hj„lpfilen p† olika s„tt&per.
:p.:link reftype=hd refid=6.Inneh†llsf”rteckning:elink.
:p.:link reftype=hd refid=7.Index:elink.
:p.:link reftype=hd refid=8.S”k:elink.
:p.:link reftype=hd refid=9.Noteringar:elink.
:p.Du kan avaktivera den h„r panelen f”r att f† mer plats&comma.
med knappen :artwork name='img6.bmp' runin.genom att v„lja Vy &endash. Visa v„nster panel fr†n
menyn&comma. eller trycka Alt&plus.P&per. G”r samma sak igen f”r att aktivera panelen&per.
:p.Du kan f”rhindra navigationspanelen fr†n att upptr„da n„r en hj„lpfil ”ppnas i
Verktyg &endash. Alternativ &endash. fliken Allm„nt&per.
:p.:hp2.Notera&colon. :ehp2. M†nga program v„ljer att visa inneh†llsf”rteckningen n„r de ”ppnar
sin hj„lpfil&semi. i det h„r fallet&comma. visas panelen automatiskt&comma. ”verskridande dina
inst„llningar&per.
:h2  id=6 res=4.Inneh†llsf”rteckning

:p.:hp2.Inneh†ll:ehp2.
:p.
:p.De flesta hj„lpfilerna har en inneh†llsf”rteckning som visar dig „mnen i filen&comma.
hierarkiskt eller som ett "tr„d"&per. Detta „r vanligtvis det f”rsta som du ser n„r du
”ppnar en fil&per.
:p.Du kan expandera eller kollapsa brancher hos tr„det genom att klicka p† &plus.
eller &endash. knapparna&comma. eller anv„nda mellanslagstangenten&per.
:p.F”r att visa ett „mne ur inneh†llet&comma. klicka bara p† det&per. Du kan ocks† flytta
runt inom inneh†llet genom att anv„nda piltangenterna&per.
:p.F”r att flytta runt genom &asterisk.alla&asterisk. „mnen inom inneh†llstr„det&comma. i ordning&comma. kan du
anv„nda Ctrl &plus. Upp och Ctrl &plus. Ner&comma. eller "F”reg†ende" :artwork name='img1.bmp' runin. och "N„sta"
:artwork name='img2.bmp' runin. knapparna&per. Detta „r
ett s„tt att behandla filen som en normal bok&comma. l„sande varje sida&per.
:p.Du kan ocks† betrakta hela inneh†llsf”rteckningen genom att anv„nda "Expandera allt" i menyn
"Vy"&per. Detta expanderar alla brancher hos inneh†llsf”rteckningen s† att du snabbt kan leta igenom
den&per. Emellertid&comma. det „r vanligtvis enklare att
anv„nda :link reftype=hd refid=8.S”k:elink. eller :link reftype=hd refid=7.Index:elink. f”r detta „ndam†l&per.
:h2 id=7 res=5.Index

:p.:hp2.Om index:ehp2.
:p.
:p.Indexfliken inneh†ller en alfabetiskt listning av „mnen eller nyckelord i hj„lpfilen&per. Du
kan snabbt s”ka igenom den endast genom att skriva n†gra inledande bokst„ver hos ordet som du
vill kolla upp&per. NewView hoppar till den f”rsta matchningen hos index automatiskt&per. F”r
att visa ett belyst „mne&comma. tryck Entertangenten&per.
:p.:hp5.Noteringar:ehp5.
:p.Hj„lpfiler kan eller kan eventuellt inte inneh†lla ett "officiellt" index&per. Index „r manuellt
skapat av f”rfattaren&comma. s† &lpar.f”r original View&rpar. „r dess anv„ndbarhet helt beroende
p† hur mycket arbete f”rfattaren har lagt ner p† det&per. Det kanske inte ens
finns n†got&per.
:p.Emellertid&comma. ett anv„ndbart index kan erbjudas genom att lista rubrikerna p† varje „mne
alfabetiskt&comma. och detta „r vad NewView g”r&per. Den sl†r sedan samman originalindex &lpar.
om det finns n†got&rpar. med listan ”ver „mnesrubriker&per.
:p.Om du av n†got sk„l inte tycker om detta&comma. kan du avaktivera det i
Verktyg &endash. Alternativ &endash. fliken Index&per.
:h2  id=8 res=6.S”k

:p.:hp2.S”k:ehp2.
:p.
:p.S”kning „r ett snabbt s„tt att finna information&comma. n„r du inte vet var du skall starta&per.
Anv„nd helt enkelt S”kfliken&comma. skriv n†gra relaterade ord och klicka p† S”kknappen&per. Du kommer
att se en listning ”ver alla „mnen inneh†llande det ordet&comma. eller ord som liknar det&comma.
med de b„sta matchningarna ”verst&per. Den b„sta matchningen kommer att
visas automatiskt&per.
:p.Ord som matchar din s”kning „r belysta i „mnet&per.
:p.:hp2.Global s”kning:ehp2.
:p.Du kan ocks† s”ka i alla hj„lpfiler p† ditt system genom att anv„nda :link reftype=hd refid=10.'GlobalSearch'.global s”kning:elink.
i Verktyg &endash. Genoms”k alla hj„lpfiler&per.
:p.:hp2.Frass”kning:ehp2.
:p.Om du vill s”ka efter en fras som best†r av fler „n ett ord&comma. l„gg till dubbla citattecken
runt den&comma. till exempel "os&slash.2 warp"&per.
:p.:hp2.Matchande funktioner:ehp2.
:p.NewView till†ter dig att finjustera din s”kning&per.
:p.&plus. indikerar ett ord som :hp2.m†ste:ehp2. matcha
:p.&endash. indikerar ett ord som :hp2.inte:ehp2. m†ste matcha
:p.NewView utf”r alltid partiella ordmatchningar&per. Det betyder&comma. om du s”ker efter "win"
kommer NewView ocks† att hitta "window" och "showing"&per.
Emellertid&comma. ju b„ttre matchning ju h”gre kommer rankningen att bli&per.
:p.:hp2.Hur NewView rankar resulten:ehp2.
:p.NewView rankar matchande „mnen p† olika s„tt&colon.
:p.&endash. en n„rmare matchning av ett helt ord
:p.&endash. antal matchande ord i ett „mne
:p.&endash. matchningar inom en rubrik
:p.&endash. matchningar inom ett index
:h2  id=9 res=7.Noteringar

:p.:hp2.L„gga till och anv„nda Noteringar:ehp2.
:p.
:p.NewView till†ter dig att l„gga till noteringar &lpar.anteckningar&rpar. till dina hj„lpfiler&per.
:p.F”r att l„gga till en notering&comma. markera helt enkelt d„r du vill l„gga till en notering
och klicka p† knappen "Notering" :artwork name='img7.bmp' runin.&comma. skriv sedan din text och
klicka p† OK&per. Texten kommer att infogas i hj„lp„mnet med en annan f„rg &lpar.standard „r gr”nt&semi. du kan „ndra detta i
Verktyg &endash. Alternativ &endash. F„rger&rpar.&per.
:p.F”r att redigera eller radera en notering&comma. klicka p† den f„rgade noterade texten&semi.
sedan kan du redigera noteringstexten&comma. eller klicka p† radera f”r att bli av med den&per.
:p.Du kan ocks† granska alla noteringar som du gjort i den aktuella hj„lpfilen&lpar.erna&rpar.
genom att g† till fliken med Noteringar&semi. d„r du kan l„gga till&comma. redigera och
radera&comma. och „ven hoppa till „mnen som inneh†ller dina noteringar&per.
:p.
:p.Noteringar sparas i en fil med „ndelsen &per.nte&comma. i samma katalog som den hj„lpfil
som de „r till f”r&per.
:p.Notera &endash. om en hj„lpfil „r „ndrad &lpar.till exempel om ett program „r uppgraderat&rpar.
kommer noteringarna inte l„ngre att upptr„da p† r„tt plats&semi.
du kan dock fortfarande l„sa dom fr†n fliken Noteringar&per.
:h1  id=10 res=18.Global s”kning

:p.:hp2.Global s”kning:ehp2.
:p.Du kan s”ka i alla dina hj„lpfiler p† ditt system&comma. genom att klicka p† knappen Global s”kning
:artwork name='img8.bmp' runin.&comma. anv„ndande menyn Verktyg &endash. Genoms”k alla hj„lpfiler&comma. eller trycka Ctrl&plus.S&per.
:p.Den h„r s”kningen fungerar liknande :link reftype=hd refid=8.s”kning inom en fil:elink.&comma.
men den talar ocks† om i vilka hj„lpfiler som resultatet hittades&per.
:p.Dessa s”kningar kan ta viss tid&comma. beroende p† vad du s”ker efter&per. Du kan stoppa
s”kningen n„r som helst&per.
:p.:hp2.D„r NewView s”ker:ehp2.
:p.Standard „r att s”ka efter hj„lpfiler i hj„lps”kv„garna&comma. vilka „r specificerade av
:link reftype=hd refid=18.milj”variablerna:elink. BOOKSHELF och HELP&per.
:p.Du kan v„lja andra platser att s”ka i genom att v„lja fr†n drop-down listen&comma. eller
anpassa listan genom att klicka p† knappen V„lj&per.
:p.:hp2.S”ka i&colon. Standard hj„lps”kv„gar:ehp2.
:p.Detta „r standard och kommer att genoms”ka BOOKSHELF och HELP som specificerats
ovanf”r&per.
:p.Klicka p† knappen V„lj kommer att till†ta dig att v„lja vilka kataloger i hj„lps”kv„garna
som skall anv„ndas&per. Klicka p† varje post i listan f”r att markera eller &endash.avmarkera
den&per. Efter att du valt detta&comma. kommer platsen att visas
som "Valda hj„lps”kv„gar"
:p.:hp2.S”ka i&colon. Alla h†rddiskenheter:ehp2.
:p.Alternativet kommer att s”ka igenom alla h†rd- &lpar.icke&endash.flyttbara&rpar. diskenheter
p† ditt system&per. Du kan klicka p† "V„lj&per.&per.&per." f”r att anpassa platsen&per.
:p.Att s”ka igenom enheter kan hitta fler hj„lpfiler&comma. men kan vara betydligt l†ngsammare „n
bara hj„lps”kv„garna&per.
:p.:hp2.S”ka i&colon. Valda hj„lps”kv„gar:ehp2.
:p.Om du redan har valt vissa hj„lps”kv„gar att s”ka i&comma. kan du klicka p† "V„lj&per.&per.&per."
f”r att anpassa igen&per.
:p.:hp2.S”ka i&colon. Kataloglista:ehp2.
:p.I dialogen "V„lj kataloger"&comma. kommer klickning p† knappen "L„gg till&per.&per.&per." att
till†ta dig att l„gga till en eller flera kataloger till
s”klistan&per.
:p.V„lj enhet och katalog anv„ndade kontrollerna som upptr„der&comma. klicka sedan p†
"L„gg till katalog" f”r att l„gga till en annan katalog&per. Du kan g”ra detta hur m†nga g†nger som
helst&per. V„lj "Med under&endash.kataloger" om du „ven vill att under&endash.kataloger till den
valda katalogen skall s”kas igenom&per. I detta fall kommer&comma. &per.&per.&per.
att dyka upp i slutet av katalogen&per.
:p.Efter att du har lagt till en anpassad katalog som denna&comma. kommer platsen f”r s”kning
att visas som "Kataloglista"&per.
:p.Notera&colon. om du l„gger till en anpassad katalog till standard eller valda hj„lps”kv„gar&comma.
kommer listan att bli en anpassad lista&comma. och du kan inte l„ngre v„lja om hj„lps”kv„gar&per.
F”r att f† tillbaka standard hj„lps”kv„gar&comma. v„lj "Standard hj„lps”kv„gar" klicka sedan p†
"V„lj&per.&per.&per." igen&per.
:p.:hp2.S”ka i&colon. Ange en plats:ehp2.
:p.Du kan ange en enhet eller katalog i inmatningsf„ltet "S”k i&colon."&per. L„gg till
"&per.&per.&per." i slutet av katalogen om
du „ven vill s”ka i underkataloger&per.
:p.Exempel&colon.
:p. S”k i&colon. &lbracket. E&colon.&bsl.mydocs&bsl.&per.&per.&per. &rbracket.
:p.Detta kommer att s”ka efter hj„lpfiler i E&colon.&bsl.mydocs&bsl.
och n†gra underkataloger&per.
:h1  id=11 res=8.Bokm„rken

:p.:hp2.Bokm„rken:ehp2.
:p.
:p.NewView till†ter dig att bokm„rka vissa „mnen inom den aktuella hj„lpfilen&per.
Klicka helt enkelt p† bokm„rkesknappen :artwork name='img9.bmp' runin. f”r att l„gga till det
aktuella „mnet som ett bokm„rke&per.
:p.F”r att hoppa till ett bokm„rke&comma. g† till menyn "Bokm„rken"&comma. och klicka p† det bokm„rke som du vill ”ppna&per.
:p.Du kan visa eller radera alla dina bokm„rken genom att klicka p† "Redigera&per.&per." i menyn
"Bokm„rken"&per. Detta f”nster kan f”rbli ”ppet medan du l„ser&comma. s† att du snabbt kan se
igenom dina bokm„rken&per.
:p.
:p.:hp5.Noteringar:ehp5.
:p.NewView's bokm„rken kommer ih†g alla „mnesf”nster som „r ”ppna&comma. om det finns
fler „n ett&per.
:p.Bokm„rken sparas i en fil med „ndelsen &per.bmk&comma. i samma katalog som den hj„lpfil
de tillh”r&per.
:h1  id=12 res=100.Internetl„nkar

:p.:hp2.Internetl„nkar:ehp2.
:p.N„r du klickar p† en webb-URL som http&colon.&slash.&slash.www&per.google&per.com&comma. kommer
NewView att starta din standard webbrowser&per.
:p.Webbrowsern „r specificerad av ditt operativsystems inst„llningar&comma. inte av NewView&per. F”r
att konfigurera den&comma. kan du ”ppna ett URL objekt p† skrivbordet&comma. redigera s”kv„gen till
browsern i fliken :hp2.Browser:ehp2.&comma. och sedan klicka p† St„ll in standard&per.
Alternativt&comma. ladda ner verktyget ConfigApps fr†n Hobbes
:p. http&colon.&slash.&slash.hobbes&per.nmsu&per.edu&slash.cgi&endash.bin&slash.h&endash.search?key&eq.configapps
.br
:p.Browsers kan ocks† ha f”rm†gan att st„lla in sig sj„lva som standard&comma.
antingen vid installationstillf„llet eller i inst„llningar&per.
:p.NewView tar hand hand om setup utf”rd av Internet-Application-Integration (IAI
)&per. E-mail, Newsgrupper och FTP-l„nkar skickas till det program som du har
konfigurerat&per. Om det inte finns n†got program f”r en specifik url-typ, kommer l„nkarna att
skickas till webbrowsern&per.
:p.:hp2.Notering till f”rfattare av hj„lpfiler:ehp2.
:p.Original View hade ingen f”rst†else f”r URL eller e-mail l„nkar&comma. s† det enda s„ttet att
implementera dem var att l„nka till&comma. exempelvis&comma.
"netscape&per.exe" med korrekta parametrar&per.
:p.NewView ”vers„tter programl„nkar mot "netscape"&comma. "explore" eller "mozilla" till l„nkar f”r
standard browser&per.
:p.Den uppt„cker ocks† automatiskt&endash. URL's i formen&colon.
:p. http&colon.&slash.&slash.x  https&colon.&slash.&slash.x ftp&colon.&slash.&slash.x
:p. mailto&colon.x  news&colon.x
:p.Saker som ser ut som URL's uppt„cks ocks†&comma. „ven utan
protokollprefix&colon.
:p. www&per.a&per.b &endash. browser
:p. ftp&per.a&per.b &endash. ftp
:p. a&atsign.b&per.c &endash. email
:p.d„r a&comma. b och c „r n†gon alfanumerisk str„ng&per.
:p.Du beh”ver inte g”ra n†got f”r att NewView skall k„nna igen dessa&per.
:h1  id=13 res=9.Kommandoradsparametrar

:p.:hp2.Kommandoradsparametrar:ehp2.
:p.N„r du k”r NewView fr†n kommandoraden kan du l„gga till olika
parametrar&per. Ingen av dom kr„vs&per.
:p.
:p.:hp2.NewView &lbracket.options&rbracket. &lbracket.&lt.filnamn&gt.
&lbracket.&lt.s”k text&gt.&rbracket.&rbracket.:ehp2.
:p.Om NewView „r installerad som en ers„ttare f”r view&comma. startar kommandot med view ist„llet f”r newview&per.
:p.:link reftype=hd refid=16.Exempel:elink.
:p.:hp2.&lt.filnamn&gt.:ehp2.
:p.Den fil som NewView skall ladda&per. Du kan ladda multipla filer p† samma g†ng genom
att anv„nda filnamn1&plus.filnamn2&comma. etc&per.
.br
:p.Om en s”kv„g inte „r specificerad&comma. kommer filerna att efters”kas i :link reftype=hd refid=18.BOOKSHELF
och HELP s”kv„garna:elink.&per.
.br
Om din s”kv„g och/eller filnamn inneh†ller specialtecken (som mellanrum) m†ste du
omsluta filnamnet med dubbla citattecken&per.
:p.:hp2.<s”k text> :ehp2.
:p.S”k „mnesrubriker och indexinneh†ll efter denna text&per. Detta „r :hp2.inte:ehp2. samma
som en normal s”kning, f”r kompatibilitet med original View&per. F”r att g”ra en ordentlig s”kning&comma.
anv„nd &slash.s alternativet &lpar.se nedanf”r&rpar.&per. F”r mer detaljer&comma. se :link reftype=hd
refid=14.Kommandorad s”k „mnen:elink.&per.
:p.:hp2.Alternativ:ehp2.
:p.:hp2.&slash.s&colon.&lt.s”k text&gt.:ehp2.
:p.Efter ”ppning av filen&comma. utf”r en :link reftype=hd refid=8.s”kning:elink. efter given text&per. (g”r en riktig
texts”kning ist„llet f”r standard „mnesrubriks”kning&per. Resultatet „r det samma som f”r att
utf”ra s”kningen fr†n :link reftype=hd refid=8. S”k navigationspanel:elink.&per.
:p.Exempel&colon.
.br
F”r att s”ka efter kopiera i hela cmdref dokumentet kan du anropa
:cgraphic.
  newview /s cmdref kopiera

:ecgraphic.
NewView „r smart nog att hantera multipla ord (som :link reftype=hd refid=8.s”k
navigationspanel:elink.)&per. Deeta „r en OR s”kning&per.
:cgraphic.
  newview /s cmdref net access

:ecgraphic.
F”r att utf”ra en AND s”kning, inneslut den s”kta frasen med dubbla citattecken&per.
:cgraphic.
  newview /s cmdref &osq.net access&osq.

:ecgraphic.

:p.:hp2./g :ehp2.
:p.Utf”r e :link reftype=hd refid=10.global s”kning:elink. efter en given text, i alla hj„lpfiler i ditt
system&per.
:p.Exempel&colon.
.br
F”r att s”ka efter kopiera i alla hj„lpfiler anv„nd
:cgraphic.
  newview /g kopiera

:ecgraphic.
S„tt in filnamnet som f”rsta parameter om du ”nskar ”ppna en hj„lpfil innan
s”kningen startas&per.
:cgraphic.
  newview /g cmdref kopiera

:ecgraphic.

:p.:hp2./? :ehp2.eller :hp2./h :ehp2.eller :hp2./hj„lp :ehp2.
:p.Visa kommandoradshj„lp
:p.Se ocks†&colon. :link reftype=hd refid=15.Avancerade parametrar:elink.
:h2 id=14 res=13.Kommandorad „mness”kning

:p.:hp2.Kommandorad s”k „mnen:ehp2.
:p. view &lt.filnamn&gt. &lt.„mne&gt.
:p.Parametern s”k „mne specificerad p† kommandoraden&comma. kopierar
beteendet hos gamla view&per.
:p.Text inom „mnen s”ks inte igenom&comma. bara rubriker och index&per. Detta g”r det mindre
anv„ndbart f”r anv„ndare&comma. men anv„nds av vissa program f”r att referera hj„lp„mnen
p† ett f”ruts„gbart s„tt&per.
:p.Du kan anv„nda multipla ord h„r&per.
:p.Den s”kning som utf”rs „r&colon.
:p.&endash. topic title startar med texts”kning
:p.&endash. index entry startar med texts”kning
:p.&endash. topic title inneh†ller s”ktext
:p.&endash. index entry inneh†ller s”ktext&per.
:p.Utvecklare b”r se till att f”rv„ntat dokument kommer att hittas anv„ndande den h„r tekniken
f”r att indentifiera „mnen vid start av nya eller gamla view&per.
:h2  id=15 res=14.Avancerade parametrar

:p.:hp2.Avancerade parametrar:ehp2.
:p.F”ljande kommandoradsparametrar „r i f”rsta hand avsedda f”r programutvecklare&comma.
men kan anv„ndas f”r n†got „ndam†l&per.
:p.:hp2.&slash.lang&colon.&lt.language spec&gt.:ehp2.
:p.Laddar det specificerade spr†ket&per. ™verskrider standardvalet baserat p† milj”variabeln
LANG&per. Till exempel&comma. &slash.lang&colon.en
:cgraphic.
  newview cmdref /lang&colon.en

:ecgraphic.
laddar English&per. Se readme&per.txt f”r mer information&per.
:p.:hp2.&slash.pos&colon.&lt.left&gt.&comma.&lt.bottom&gt.&comma.&lt.width&gt.&comma.&lt.height&gt.:ehp2.
:p.St„ller in huvudprogramf”nstret till en given position och storlek&per. Alla v„rden m†ste
anges&per. S„tt ett :hp2.P:ehp2. efter ett nummer f”r att specificera en procentsats&per. Till
exempel&colon.
:cgraphic.
  newview /pos&colon.10P,10P,80P,80P

:ecgraphic.

:p.centrerar f”nstret och t„cker 80&percent. av sk„rmstorleken&per.
:p.:hp2.&slash.title&colon.&lt.window title&gt.:ehp2.
:p.St„ller in rubriken hos NewView's f”nster till en specificerad text&comma. ”verskridande
vad som upptr„der i hj„lpfilen&per. Texten "Help &endash.  " kommer alltid att infogas framf”r
den specificerade texten&comma. om inte den specificerade texten „r "help"&comma. i vilket fall
rubriken helt enkelt kommer att bli "Help"&per. Detta „r f”r att f”rs„kra att hj„lpf”nstret
alltid upptr„der som ett s†dant i listan ”ver ”ppna f”nster&per.
:p.Om du beh”ver specificera multipla ord&comma. omge hela alternativet med citattecken&comma.
till exempel&colon.
:cgraphic.
newview cmdref &osq./title&colon.Kommandorad Hj„lp&osq.

:ecgraphic.

:h2  id=16 res=15.Kommandorad exempel

:p.:hp2.Kommandorad exempel:ehp2.
:p.F”ljande exempel utg†r ifr†n att newview „r installerad som en komplett ers„ttning
och d„rmed „r view i verkligheten newview&per.
:p.:hp2.view cmdref:ehp2.
:p.™ppna filen cmdref&per.inf &lpar.OS&slash.2 Command Reference&rpar. fr†n hj„lps”kv„gen&per.
:p.:hp2.view cmdref&plus.os2ug:ehp2.
:p.™ppna tv† filer&comma. cmdref&per.inf och os2ug&per.inf &lpar.OS&slash.2 User
Guide&rpar.&comma. i samma f”nster&per.
.br
:p.Inneh†llsf”rteckningen fr†n os2ug&per.inf „r adderad i slutet av inneh†llet fr†n
cmdref&per.inf&per. Alla index „r kombinerade alfabetiskt&per.
:p.:hp2.view c&colon.&bsl.os2&bsl.book&bsl.os2ug&per.inf:ehp2.
:p.™ppna filen os2ug&per.inf i c&colon.&bsl.os2&bsl.book katalogen&per.
:p.:hp2.view &osq.c&colon.&bsl.os2 book&bsl.os2ug&per.inf&osq. :ehp2.
:p.Omge path&bsl.filen med dubbla citattecken om den inneh†ller specialtecken
(som mellanrum)&per.
:p.:hp2.view cmdref dir:ehp2.
:p.™ppna filen cmdref &lpar.OS&slash.2 command reference&rpar. och leta i rubriker och
index efter ordet "dir"&per. Kommer att visa hj„lpsidan f”r DIR kommandot&per.
:p.:hp2.view &slash.s&colon.desktop os2ug:ehp2.
:p.™ppna filen os2ug&per.inf och s”k efter ordet "desktop"&per. Den b„sta matchningen
kommer att visas&per.
:p.:hp2.view &slash.g&colon.permissions:ehp2.
:p.Utf”r en s”kning i alla hj„lpfiler efter ordet "permissions"&per.
:p.:hp2.set myhelp&eq.cmdref&plus.os2ug&plus.rexx:ehp2.
.br
:p.:hp2.view myhelp:ehp2.
:p.Den f”rsta raden st„ller in en milj”variabel MYHELP att inneh†lla namnen hos tre
hj„lpfiler&per. Den andra raden ”ppnar de tre filerna&per.
:h1  id=17 res=10.Tangentbord genv„gar

:p.:hp2.Tangentbord genv„gar:ehp2.
:p.De flesta tangentbordsgenv„gar „r synliga i menyn&comma. men n†gra f† syns inte&per.
De ytterligare genv„garna „r&colon.
:p.:hp2.Alt&plus.F4:ehp2. Avsluta
:p.:hp2.Ctrl&plus.C:ehp2. Kopiera vald text till klippbordet
:p.:hp2.F7:ehp2. Bak†t
:p.:hp2.F8:ehp2. Fram†t
:p.:hp2.Ctrl&plus.Left:ehp2. Bak†t
:p.:hp2.F11:ehp2. F”reg†ende i inneh†ll
:p.:hp2.F12:ehp2. N„sta i inneh†ll
:p.
:p.:hp2.Genv„gar synliga i menyn:ehp2.
:p.:hp2.Ctrl&plus.O:ehp2. ™ppna filer
:p.:hp2.Ctrl&plus.E:ehp2. ™ppna filer fr†n hj„lps”kv„gar
:p.:hp2.Ctrl&plus.N:ehp2. ™ppna ett nytt f”nster
:p.:hp2.Ctrl&plus.P:ehp2. Skriv ut „mne
:p.:hp2.F3:ehp2. Avsluta
:p.
:p.
:p.:hp2.Ctrl&plus.A:ehp2. V„lj all text i ett „mne
:p.:hp2.Ctrl&plus.Ins:ehp2. Kopiera vald text till klippbordet
:p.
:p.:hp2.Ctrl&plus.F:ehp2. Finn inom aktuellt „mne
:p.:hp2.Ctrl&plus.G:ehp2. Repetera senaste finn
:p.
:p.:hp2.Ctrl&plus.S:ehp2. ™ppna globalt s”kningsverktyg
:p.
:p.:hp2.Alt&plus.C:ehp2. Žndra till inneh†llsfliken
:p.:hp2.Alt&plus.I:ehp2. Žndra till indexfliken
:p.:hp2.Alt&plus.S:ehp2. Žndra till s”kfliken
:p.:hp2.Alt&plus.N:ehp2. Žndra till noteringsfliken
:p.:hp2.Alt&plus.P:ehp2. V„xla v„nster panel &lpar.flikar&rpar. p† och av
:p.:hp2.F5:ehp2. Expandera allt inneh†ll
:p.:hp2.F6:ehp2. Kollapsa allt inneh†ll
:p.
:p.:hp2.Esc:ehp2. Bak†t
:p.:hp2.Ctrl&plus.Right:ehp2. Fram†t
:p.:hp2.Ctrl&plus.Up:ehp2. F”reg†ende „mne i inneh†ll
:p.:hp2.Ctrl&plus.Down:ehp2. N„sta „mne i inneh†ll
:p.
:p.:hp2.Ctrl&plus.D:ehp2. Redigera bokm„rken
:p.:hp2.Ctrl&plus.B:ehp2. Bokm„rk aktuellt „mne
:p.
:p.:hp2.Ctrl&plus.M:ehp2. L„gg till notering vid pekarens position
:p.
:p.:hp2.F1:ehp2. Hj„lp f”r NewView
:h1  id=18 res=11.Milj”variabler

:p.:hp2.Milj”variabler:ehp2.
:p.
:p.B†de :hp2.BOOKSHELF:ehp2. och :hp2.HELP:ehp2. milj”variablerna definierar s”kv„gar &lpar.”ver
kataloger&rpar. f”r s”kning efter hj„lpfiler&per. NewView anv„nder b†da s”kv„garna utan
†tskillnader&per.
:p.Dessa s”kv„gar s”ks igenom n„r du&colon.
:p.o specificerar en hj„lpfil utan en s”kv„g fr†n kommandoraden
:p.o anv„nder menyalternativet Arkiv &endash. ™ppna Special&per.&per.&per.
:p.o g”r en :link reftype=hd refid=10.Global s”kning:elink.
:p.Du kan permanent l„gga till kataloger med hj„lpfiler i :hp2.HELP:ehp2. eller :hp2.BOOKSHELF:ehp2.
s”kv„garna genom att modifiera CONFIG&per.SYS filen&per. L„gg till i b†da s”kv„garna&comma.
om du „ven vill att det gamla viewprogrammet skall kunna hitta filerna&per.
:p.:hp2.Andra milj”variabler:ehp2.
:p.Milj”variabeln :hp2.LANG:ehp2. utv„rderas f”r att best„mma standardspr†k som  NewView skall visas
i&per. &lpar.™verskrids av &slash.lang :link reftype=hd refid=15.kommandoradsparametern:elink.&per.
&rpar. Se newview readme&per.txt f”r mer information om spr†k&per.
:p.Den katalog som definieras i :hp2.LOGFILES:ehp2. anv„nds f”r att logga krasher eller annan
information&per.
:p.Subkatalogen "lang" under den katalog som definieras av :hp2.OSDIR:ehp2. s”ks igenom efter
spr†kfiler vid starten&per.
:p.S”kv„gen :hp2.ULSPATH:ehp2. s”ks ocks† igenom efter spr†kfiler&per.
:h1  id=19 res=20.F”r f”rfattare och utvecklare

:p.:hp2.F”r f”rfattare och utvecklare:ehp2.
:p.
:p.Den h„r sektion t„cker n†gra „mnen f”r dokumentf”rfattare och programutvecklare&per.
:p.Se ocks† sektionen om URL igenk„nnande
i :link reftype=hd refid=12.Internet l„nkar:elink.&per.
:h2 id=20 res=12. Skriva hj„lpfiler

:p.:hp2. Skriva OS/2 hj„lpfiler:ehp2.
:p.
:p.OS&slash.2 hj„lpfiler produceras med anv„ndning av IPF Compiler&per. IPF st†r f”r
Information Presentation Facility&per.
:p.IPF Compiler tar en textfil skriven i ett spr†k som talar om f”r den om saker som headings&comma.
l„nkar&comma. text och bilder&comma.och producerar antingen en &per.INF eller
&per.HLP fil&per.
:p.Den officiella v„gen att skaffa IPF kompilatorn &lpar.ipfc&per.exe&rpar. „r fr†n
OS&slash.2 Developers Toolkit&per. Denna inkluderas fritt med eComStation
&lpar.http&colon.&slash.&slash.www&per.ecomstation&per.com&rpar.&per.
:p.Eftersom spr†ket f”r IPFC „r tr†kigt &lpar.till exempel m†ste all punktation skrivas i form av
speciella nyckelord&comma. som &amp.comma&per.&rpar. anv„nder m†nga m„nniskor andra verktyg
utanf”r sj„lva IPF kompilatorn&per.
:p.Jag anv„nder Vyperhelp &lpar.http&colon.&slash.&slash.www&per.vyperhelp&per.com&rpar. eftersom
den „r enkel och grafisk&per. Den kan ocks† exportera till Windowshj„lp&comma. HTML och andra&comma.
trots att den bara kan k”ras p† OS&slash.2&per. Den „r numera :hp2.Freeware:ehp2.&per.
:p.Vissa andra popul„ra alternativ „r&colon.
:p.o HyperText&slash.2 IPF Preprocessor
&lpar.http&colon.&slash.&slash.www&per.clanganke&per.de&slash.os2&slash.sw&slash.htext&slash.&rpar.
.br
&endash. F”rprocessar ett enklare startspr†k till det v„ldigt sv†ra IPF formatet&per.
Fritt&per.
:p.o HyperMake &lpar.http&colon.&slash.&slash.www&per.hypermake&per.com&rpar.&per.
Liknande&comma. men kan ocks† producera Windowshj„lp och HTML&per.
:p.o Sibyl &lpar.vilken NewView skapades med&rpar. kommer med en IPF f”rprocessor&per.
:p.o IPFEditor fr†n PCS &lpar.http&colon.&slash.&slash.www&per.pcs&endash.soft&per.com&slash.productipfe212&per.htm&rpar.
&per.
.br
Troligtvis den mest kompletta&comma. men relativt kostsam&comma. och l†ngt ifr†n felfri&per.
Notera&colon. NewView st”der inte allt som IPFE kan g”ra&xclm.
.br
:p.Tidigare fanns det m†nga andra alternativ&per. De listade skall fortfarande vara tillg„ngliga
och ha viss support&per.
:h2  id=21 res=16.Žmnen resource IDs

:p.:hp2.Žmnen resource IDs:ehp2.
:p.Resource ID's anv„nds av f”rfattare av onlinehj„lp f”r applikationer&comma. f”r att identifiera
hj„lp„mnen&per. Applikationer anropar Help Manager speciferande ett resource ID&comma. antingen
direkt anv„ndande HM&us.DISPLAY&us.HELP meddelande&comma. eller indirekt via hj„lptabeller som lagts
till resources&comma. vilka hanterar PM automatiskt&per. Resource ID's lagras i en tabell inom
hj„lpfilen&per.
:p.F”r dokumentf”rfattare&comma. erbjuder NewView m”jligheten att se och hitta resource
IDs&per.
:p.:hp2.Finna genom resource ID:ehp2.
:p.Anv„nd Verktyg &endash. Hitta „mne med hj„lp av resource ID f”r att s”ka efter ett specificerat
resource ID i alla ”ppnade filer&per.
:p.:hp2.Visa resource ID's:ehp2.
:p.Anv„nd „mnen egenskaper &lpar.h”ger musklick &endash. Egenskaper&rpar. f”r att se vilka
resource ID's som „r associerade med ett „mne&per.
:h2  id=22 res=19.Žmnen namn

:p.:hp2.Žmnen namn:ehp2.
:p.
:p.Liksom resource IDs&comma. kan „mnesnamn anv„ndas av utvecklare f”r att l„nka till hj„lp„mnen
inom deras applikation&comma. genom att anv„nda HM&us.DISPLAY&us.HELP meddelandet med parameter 2 HM&us.PANELNAME&per.
:p.Detta anv„nds inte s„rskilt ofta&per.
:p.NewView kan hitta ett speciellt „mne&comma. genom att anv„nda Verktyg &endash. Finn „mnesnamn&per.
:euserdoc.
