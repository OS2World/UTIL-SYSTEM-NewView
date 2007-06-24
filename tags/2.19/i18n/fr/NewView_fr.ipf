:userdoc.
:docprof toc=123456.
.nameit symbol=prodname text=':hp2.:color fc=default bc=default.NewView:color fc=default bc=default.:ehp2.'
:title.Aide de NewView
.* Subject: Help for &prodname.
.* Version:
.* Copyright: Copyright 2005 Aaron Lawrence
.* Author: Aaron Lawrence
.* French translation : Guillaume Gay
.* French version : 2007-05-25

.**************************************************************************
.* SECTION : INTRODUCTION                                                 *
.**************************************************************************
:h1 res=30000 id='Introduction'.Introduction
:i1 id=30001.Support
:artwork runin name='..\images\newview.bmp'.
:hp2.Bienvenue dans :ehp2.&prodname.:hp2. &xclm. :ehp2.
:p.
:artwork align=center name='newview.bmp'.
:p.
:p.&prodname. est un programme qui permet de lire les fichiers d'aide
d'OS&slash.2 &lpar.ou eComStation&rpar.&per.
:p.:link reftype=hd refid='Support'.Licence et support:elink.
:p.:link reftype=hd refid='Utilisation'.Utilisation de &prodname.:elink.
:p.:hp1.Pour �viter que ce fichier apparaisse lorsque vous lancez
&prodname. sans sp�cifier de fichier d'aide, allez sur l'onglet G�n�ral
dans Outils &endash. Options&per. :ehp1.
:p.:hp2.Historique:ehp2.
:p.&prodname. se substitue au programme original d'IBM fourni avec
OS&slash.2&per.
:p.Il am�liore plusieurs aspects de View&comma. avec une interface
plus moderne et facile � utiliser&comma. plus d'options&comma.
et de nouvelles fonctionnalit�s que View n'avait tout simplement
pas&per.
:p.
.**************************************************************************
.* SECTION : SUPPORT AND LICENSING                                        *
.**************************************************************************
:h2 res=1 id='Support'.Licence et support
:i1 id=30002.Licence
:i2 refid=30001.Licence et support
:i2 refid=30001.Introduction
:i1 id=30003.Bugs
:i1 id=30004.Code source
:p.:hp2.Licence et support:ehp2.
:p.&prodname. est un copyright &bx0012. 1999&endash.2006 Aaron Lawrence&per.
Il est aussi  distribu� sous licence publique GNU (GNU Public
License ou GPL)&comma. ce qui signifie que vous avez le droit
d'obtenir le code source&per.
:p.Depuis 2006, Ronald Brill maintient le produit&per.
:p.&prodname. est un projet Netlabs&per.
:p.Veuillez consulter le fichier LisezMoi.txt pour plus de d�tails
techniques&per.
:p.Veuillez consulter le fichier Changes&per.txt pour un
historique des modifications apport�es � &prodname.&per.
:p.Si vous trouvez &prodname. utile&comma. veuillez m'envoyer un M�l
et&slash.ou faire un don pour supporter les d�veloppements futurs&per.
C'est toujours sympa. de recevoir des nouvelles ! Vous pourrez
alors &colon.
:ul compact.
:li.faire vos suggestions, compliments ou rapports de bugs
ici &colon. http&colon.&slash.&slash.svn.netlabs.org&slash.newview
:li.nous faire parvenir la traduction de &prodname. dans votre langue
:li.faire un don � Netlabs &colon. http&colon.&slash.&slash.www.mensys.nl
:eul.
:p.:hp2.Rapport de bugs:ehp2.
:p.Si vous deviez reporter un plantage ou autre probl�me, soyez alors
aussi pr�cis que possible quant aux fichiers qui �taient utilis�s, sur
ce que vous faisiez, etc.&per. S'il est disponible, vous �tes PRI�(E)
d'inclure le fichier newview.log. Ce fichier de rapport se trouve SOIT
:ul compact.
:li.dans le m�me r�pertoire que celui de &prodname.,
:li.dans le dossier d�fini dans la variable d'environnement
:hp2.LOGFILES:ehp2. (dans le cas des versions 1.1 et ult�rieures de eCS).
:eul.
Si le probl�me est sp�cifique � un fichier d'aide particulier, envoyez-le
moi, du moment qu'il n'est pas trop gros (taille sup�rieure � 1 Mo).
:p.La plupart des informations se trouve dans le fichier
:hp2.newview&per.log:ehp2., mais ce serait tr�s utile si vous pouviez le
donner pour v�rification &colon.
:ol compact.
:li.La version de &prodname. &lpar.":hp2.Aide:ehp2." &endash. ":hp2.Info.
produit:ehp2."&rpar.&comma.
:li.les noms des fichiers d'aide&per.
:eol.
:p.Une copie d'�cran pourrait se r�v�ler utile s'il s'agit d'un affichage
incorrect ou corrompu.
:p.:hp2.Pourquoi mon fichier d'aide ne fonctionne-t-il pas correctement ? :ehp2.
:p.Certaines des fonctionnalit�s les moins utilis�es du programme View
original non pas �t� impl�ment�es. C'est soit parceque je ne l'ai pas
vues, soit parceque cela ne vaut pas le peine d'y perdre du temps. Comme
exemples, il y a les m�tafichiers, l'index des synonymes, l'API de
contr�le dans son int�gralit�, et ainsi de suite.
:p.Malheureusement, il semblerait qu'au moins un d�veloppeur a utilis�
une ou plusieurs de ces fonctionnalit�s, il vous arrivera donc
occasionnellement de trouver quelques fichiers qui ne se chargent pas
ou qui ne fonctionnent pas correctement.
:p.
.**************************************************************************
.* SECTION : USAGE                                                        *
.**************************************************************************
:h1 res=2 id='Utilisation'.Utilisation de NewView
:p.:hp2.Utilisation de &prodname.:ehp2.
:p.Apr�s avoir :link reftype=hd refid='OuvertureDeFichiers'.ouvert un
fichier:elink.&comma. vous pouvez le lire de diff�rentes mani�res.
:p.Vous pouvez consulter la :link reftype=hd refid='Contenu'.table
des mati�res:elink.&comma. utiliser l':link reftype=hd refid='Index'.index
alphab�tique:elink. ou la
:link reftype=hd refid='Recherche'.recherche:elink.&per.
:p.Pour lire simplement le fichier d'aide comme s'il s'agissait d'un
livre, utilisez les boutons ":hp2.Pr�c�dent:ehp2."
:artwork runin name='..\images\previous.bmp'. et ":hp2.Suivant:ehp2."
:artwork runin name='..\images\next.bmp'. pour parcourir une � une
toutes les sections.
:p.Vous pouvez aussi naviguer dans le fichier d'aide comme sur le Web en
utilisant les boutons ":hp2.Arri�re:ehp2."
:artwork runin name='..\images\back.bmp'. et ":hp2.Avant:ehp2."
:artwork runin name='..\images\forward.bmp'. pour retourner l� o� vous
�tiez avant ou bien retourner sur vos pas.
:p.Les couleurs et certains comportements de &prodname. peuvent �tre
personnalis�s via l'�l�ment ":hp2.Options:ehp2." du menu
":hp2.Outils:ehp2.".
:p.Vous pouvez en outre :link reftype=hd refid='Notes'.annoter:elink. ou
:link reftype=hd refid='Signets'.mettre en signet:elink. des sections.
:p.
.**************************************************************************
.* SECTION : OPENING FILES                                                *
.**************************************************************************
:h1 res=3 id='OuvertureDeFichiers'.Ouverture de fichiers
:i1 id=30005.Ouvrir
:p.:hp2.Ouverture de fichiers d'aide:ehp2.
:p.Pour ouvrir un fichier d'aide, vous pouvez utiliser l'une des
m�thodes suivantes &colon.
:ul compact.
:li.en double-cliquant sur une :link reftype=hd refid='IconesAide'.ic�ne
d'aide:elink. d�j� param�tr�e&comma.
:li.en tapant "view :hp1.NomFichier:ehp1." depuis la
:link reftype=hd refid='LigneDeCommandes'.ligne de commandes:elink.&comma.
:li.en cliquant sur le bouton "Ouvrir..."
:artwork runin name='..\images\open.bmp'. dans &prodname.&comma.
:li.en rechargeant un fichier ouvert r�cemment depuis le menu
"Fichier"&comma.
:li.par glisser&slash.d�poser d'un fichier d'aide depuis le bureau.
Une fois le fichier charg�, vous devriez voir s'afficher la
:link reftype=hd refid='Contenu'.table des mati�res:elink. et la
premi�re section.
:eul.
:note.il est suppos� ici que vous avez install� &prodname. comme
remplacement du programme View original. Si ce n'est pas le cas, les
ic�nes d'aide existantes et la ligne de commandes peuvent se comporter
diff�remment.
:p.:hp2.Chargement simultan� de plusieurs fichiers:ehp2.
:p.&prodname. peut charger plusieurs fichiers simultan�ment, en les
pr�sentant comme s'ils n'�taient qu'un seul et rechercher des noms de
fichiers dans les variables d'environnement.
:p.Par exemple, dans la documentation du Kit de D�veloppement d'OS&slash.2
("OS&slash.2 Developer&apos.s Toolkit") &colon.
.br
  :font facename='Courier' size=14x8.NewView cpref:font facename='Default' size=0x0.
.br
charge le Guide et r�f�rence du programme de contr�le ("Control Program
Guide and Reference"). :hp2.CPREF:ehp2. est une variable d'environnement
param�tr�e dans le fichier :hp2.CONFIG.SYS:ehp2. et consiste en
"CP1&plus.CP2&plus.CP3" indiquant � &prodname. &lpar.ou
:hp2.View:ehp2.&rpar. qu'il faut charger les fichiers d'aide CP1, CP2 et
CP3. Les fichiers sont recherch�s dans les chemins sp�cifi�s dans deux
:link reftype=hd refid='VariablesEnvironnement'.variables
d'environnement:elink.&per.
:p.Les fichiers sont tous charg�s et effectivement ajout�s les uns aux
autres.
:p.�tant capable de charger ainsi plusieurs fichiers peut se r�v�ler
utiles pour plusieurs raisons. Par exemple, :hp2.4OS&slash.2:ehp2.
&lpar.une alternative � :hp2.CMD&per.EXE:ehp2.&rpar. utilise cette
technique pour ajouter sa propre aide par-dessus l'aide de CMD originale.
Vous pouvez le faire vous-m�me avec les fichiers que vous voulez.
:p.Vous pouvez charger plusieurs fichiers dans la bo�te de dialogue
d'ouverture de fichiers en utilisant la touche :hp2.Ctrl:ehp2. ou la
touche :hp2.Maj:ehp2. pour s�lectionner un ensemble de fichiers.
:p.Lorsque vous cliquez sur un lien vers un fichier d'aide diff�rent,
&prodname. charge l'autre fichier sans fermer le fichier en cours.
:p.� n'importe quel moment vous pouvez savoir quels sont les fichiers
ouverts en utilisant l'option ":hp2.Information...:ehp2." du menu
":hp2.Fichier:ehp2.".
:p.:hp2.Chargement de fichiers suppl�mentaires:ehp2.
:p.Vous pouvez cocher la case ":hp2.Conserver les fichiers ouverts:ehp2."
dans la bo�te de dialogue d'ouverture de fichiers, et &prodname. ouvrira
le(s) fichier(s) que vous avez s�lectionn�(s) sans fermer les fichiers en
cours.
:p.:hp2.Glisser&slash.d�poser:ehp2.
:p.Vous pouvez glisser et d�poser des fichiers &per.INF ou &per.HLP sur
&prodname. pour les ouvrir. Si vous maintenez appuy� la touche
:hp2.Maj:ehp2., ils seront ouverts sans que les fichiers en cours ne
soient ferm�s.
:p.Vous pouvez d�poser des fichiers � n'importe quel endroit de la
fen�tre de contenu comme les fen�tres de table des mati�res ("Contenu") ou
d'Index, ou une fen�tre de section existante.
:note.certains liens de fichiers � fichiers ne fonctionneront que si
l'ensemble des fichiers est correctement charg�.
:p.
.**************************************************************************
.* SECTION : HELPICONS                                                    *
.**************************************************************************
:h2 res=17 id='IconesAide'.Ic�nes d'aide
:p.:hp2.Ic�nes d'aide:ehp2.
:p.Les ic�nes d'aide sur le bureau sont en g�n�ral des objets "programme"
ayant pour nom de fichier ":hp2.VIEW&per.EXE:ehp2." et pour param�tres
les noms des fichiers d'aide.
:p.Certains programmes cr�ent ces ic�nes automatiquement au moment de
l'installation.
:p.Vous pouvez cr�er ces ic�nes vous-m�me en utilisant le mod�le d'objet
programme du bureau. Veuillez vous r�f�rer � l'aide du bureau pour plus
d'informations.
:p.Si vous cr�ez des ic�nes en glissant des fichiers d'aide sur le
bureau, vous ne pourrez alors pas leur donner un titre plus significatif,
puisque cela changerait le nom du fichier, emp�chant alors les
programmes de trouver leurs fichiers d'aide. L'usage d'un objet
programme est donc le moyen recommand� pour cr�er des ic�nes d'aide.
:p.
.**************************************************************************
.* SECTION : NAVIGATION PANEL                                             *
.**************************************************************************
:h1 res=200 id='PanneauDeNavigation'.Onglets du panneau de navigation
:p.:hp2.Onglets du panneau de navigation:ehp2.
:p.Le panneau de gauche comprends plusieurs onglets pour naviguer de
diff�rentes mani�res dans le fichier d'aide en cours.
:ul compact.
:li.:link reftype=hd refid='Contenu'.Onglet Contenu &lpar.table des
mati�res&rpar.:elink.
:li.:link reftype=hd refid='Index'.Onglet Index:elink.
:li.:link reftype=hd refid='Recherche'.Onglet Recherche:elink.
:li.:link reftype=hd refid='Notes'.Onglet Notes:elink.
:eul.
:p.Vous pouvez d�sactiver ce panneau afin d'avoir plus de place, en
s�lectionnant l'option ":hp2.Panneau de gauche:ehp2." du menu
":hp2.Affichage:ehp2.", ou en appuyant sur la combinaison de touches
:hp2.Alt:ehp2.&plus.:hp2.P:ehp2.&per. R�p�tez l'op�ration pour l'activer �
nouveau.
:p.Vous pouvez emp�cher le panneau de navigation d'appara�tre lorsqu'un
fichier d'aide est ouvert sur l'onglet ":hp2.G�n�ral:ehp2." des
":hp2.Options:ehp2." du menu ":hp2.Outils:ehp2.".
:note.pas mal de programmes choisissent d'afficher la table des mati�res
lorsqu'ils ouvrent leurs fichiers d'aide &semi. dans ce cas, le panneau
est alors automatiquement affich�, ignorant vos param�tres.
:p.
.**************************************************************************
.* SECTION : CONTENTS                                                     *
.**************************************************************************
:h2 res=4 id='Contenu'.Contenu
:i1 id=30006.Contenu
:hp2.Table des mati�res ("Contenu"):ehp2.
:p.:artwork align=center  name='contentstab.bmp'.
:p.La plupart des fichiers d'aide a une table des mati�res qui affiche
les diff�rents chapitres (ou sections) du fichier, sous forme hi�rarchique
ou d'arbre. C'est en g�n�ral la premi�re vue que vous obtenez lorsque vous
ouvrez ces fichiers.
:p.Vous pouvez d�rouler ou enrouler les branches de l'arbre en cliquant
sur les boutons :hp2.&plus.:ehp2. ou :hp2.&endash.:ehp2.&comma. ou en
utilisant la barre espace.
:p.Pour afficher une section depuis la fen�tre Contenu, cliquez juste
dessus. Vous pouvez aussi vous d�placer dans le contenu en utilisant les
touches fl�ches.
:p.Pour se d�placer � travers &asterisk.toutes&asterisk. les sections
de la table des mati�res, dans l'ordre, vous pouvez utiliser les
combinaisons de touches :hp2.Ctrl:ehp2.&plus.:hp2.Haut:ehp2. et
:hp2.Ctrl:ehp2.&plus.:hp2.Bas:ehp2.&comma. ou les boutons
":hp2.Pr�c�dent:ehp2." :artwork runin name='..\images\previous.bmp'. et
":hp2.Suivant:ehp2." :artwork runin name='..\images\next.bmp'.&per. C'est une
mani�re de traiter le fichier comme s'il s'agissait d'un livre, en
feuilletant une � une chaque page.
:p.Vous pouvez consulter la table des mati�res compl�te en utilisant
l'option ":hp2.Tout d�rouler:ehp2." du menu ":hp2.Affichage:ehp2.". Cela
d�roulera toutes les branches de la table des mati�res de mani�re � ce que
vous puissiez la parcourir plus rapidement. Cependant, il est en g�n�ral
plus ais� d'utiliser la
:link reftype=hd refid='Recherche'.recherche:elink. ou
l':link reftype=hd refid='Index'.index:elink. � cet effet.
:p.
.**************************************************************************
.* SECTION : INDEX                                                        *
.**************************************************************************
:h2 res=5 id='Index'.Index
:p.:hp2.Index:ehp2.
:p.:artwork align=center name='indextab.bmp'.
:p.L'onglet :hp2.Index:ehp2. contient la liste alphab�tique des sections
ou mots-cl�s du fichier d'aide. Vous pouvez y effectuer une recherche
rapide en tapant les premiers caract�res du mot d�sir�. &prodname. ira
automatiquement � la premi�re correspondance trouv�e dans l'index. Pour
visualiser la section s�lectionn�e, appuyez sur la touche
:hp2.Entr�e:ehp2.&per.
:nt text='Remarques &colon.'.
:p.les fichiers d'aide peuvent ou non inclure un index "officiel". L'index
est cr�� manuellement par l'auteur, son utilit� &lpar.pour le programme
View d'origine&rpar. est donc strictement d�pendante de la quantit� de
travail que l'auteur a mis dans le fichier. Il se peut qu'il n'y en ait
m�me pas.
:p.Cependant, un index utilisable peut �tre fourni simplement en listant
alphab�tiquement les titres de chaque section, ce que &prodname. fait.
Il fusionne alors l'index original &lpar.s'il y en a un&rpar. avec la
liste des titres de sections.
:p.Si, pour quelque raison que ce soit, vous n'aimez pas, vous pouvez
d�sactiver cette fen�tre dans l'onglet ":hp2.G�n�ral:ehp2." des
":hp2.Options:ehp2." du menu ":hp2.Outils:ehp2.".
:ent.
:p.
.**************************************************************************
.* SECTION : SEARCH                                                       *
.**************************************************************************
:h2 res=6 id='Recherche'.Recherche
:p.:hp2.Recherche:ehp2.
:p.:artwork align=center name='searchtab.bmp'.
:p.Recherche est un moyen rapide pour trouver une information lorsque
vous ne savez pas par o� commencer. Allez tout simplement dans l'onglet
:hp2.Recherche:ehp2.&comma. tapez les mots relatifs � votre recherche et
cliquez sur le bouton ":hp2.Recherche:ehp2.".
:p.Vous verrez une liste de toutes les sections contenant ce mot, ou des
mots approchants, les r�sultats les plus pertinents �tant en haut de la
liste. La meilleure correspondance sera automatiquement affich�e.
:p.Les mots qui correspondent � votre recherche sont mis en
surbrillance dans la section.
:p.:hp2.Recherche globale:ehp2.
:p.Vous pouvez aussi effectuer une recherche dans tous les fichiers d'aide
de votre syst�me en utilisant la
:link reftype=hd refid='RechercheGlobale'.Recherche globale:elink. via
l'option ":hp2.Rechercher tous les fichiers d'aide:ehp2." dans le menu
":hp2.Outils:ehp2.".
:p.:hp2.Recherche de phrase:ehp2.
:p.Si vous voulez rechercher une phrase compos�e de plus d'un mot,
encadrez-la par des guillemets, par exemple "OS&slash.2 warp".
:p.:hp2.Fonction de correspondance:ehp2.
:p.&prodname. vous permet un contr�le plus fin sur la recherche.
:p.:hp2.&plus.:ehp2. pr�cise qu'un mot :hp2.doit:ehp2. correspondre
exactement dans la recherche.
:p.:hp2.&endash.:ehp2. pr�cise qu'un mot :hp2.ne:ehp2. doit :hp2.pas:ehp2.
forc�ment correspondre dans la recherche.
:p.&prodname. est toujours partial dans la correspondance des mots. Ceci
dit, si vous recherchez "cher", &prodname. vous trouvera aussi "chercher" et
"m�cher". Toute fois, meilleure sera la correspondance, plus haut elle se
trouvera dans la liste.
:p.:hp2.Comment :ehp2.&prodname.:hp2. �value-t-il ses r�sultats ? :ehp2.
:p.&prodname. �value les sujets correspondants avec divers moyens &colon.
:ul compact.
:li.une correspondance avec le mot entier,
:li.le nombre de mots correspondants dans une section,
:li.correspondances dans le titre,
:li.correspondances dans les entr�es d'index.
:eul.
:p.
.**************************************************************************
.* SECTION : NOTES                                                        *
.**************************************************************************
:h2 res=7 id='Notes'.Notes
:p.:hp2.Ajout et utilisation de notes:ehp2.
:p.:artwork align=center name='notestab.bmp'.
:p.&prodname. vous permet de placer des notes dans vos fichiers d'aide.
:p.Pour ajouter une note, cliquez simplement � l'endroit o� vous voulez
placer une note, cliquez sur le bouton ":hp2.Note:ehp2."
:artwork runin name='..\images\note.bmp'.&comma. puis tapez votre texte et
appuyez sur la touche ":hp2.Entr�e:ehp2."&per. Le texte sera ins�r� dans
la section de l'aide avec une couleur diff�rente &lpar.verte par
d�faut &semi. vous pouvez la changer sur l'onglet ":hp2.Couleurs:ehp2."
des ":hp2.Options:ehp2." du menu ":hp2.Outils:ehp2."&rpar.&per.
:p.Pour �diter ou supprimer une note, cliquez sur le texte color� de la
note &semi. vous pouvez alors �diter le texte de cette note, ou cliquer
sur ":hp2.Supprimer:ehp2." pour vous en d�barrasser.
:p.Vous pouvez aussi voir toutes les notes que vous avez faites dans
le(s) fichier(s) d'aide en allant sur l'onglet ":hp2.Notes:ehp2." &semi. il
vous permet d'en ajouter, �diter et supprimer, ainsi que de vous rendre �
la section correspondant � votre note.
:p.Les notes sont sauvegard�es dans un fichier portant l'extension
&per.NTE, situ� dans le m�me r�pertoire que le fichier d'aide auquel elles
se rapportent.
:note.si un fichier d'aide est modifi� &lpar.par exemple si un programme
est mis � jour&rpar. les notes n'appara�tront alors plus au bon
endroit &semi. cependant, vous pourrez toujours les consulter dans
l'onglet :hp2.Notes:ehp2.&per.
:p.
.**************************************************************************
.* SECTION : GLOBAL SEARCH                                                *
.**************************************************************************
:h1 res=18 id='RechercheGlobale'.Recherche globale
:p.:hp2.Recherche globale:ehp2.
:p.Vous pouvez effectuer une recherche dans tous les fichiers de votre
syst�me en cliquant sur le bouton ":hp2.Recherche globale:ehp2."
:artwork runin name='..\images\search.bmp'.&comma. en utilisant l'option
":hp2.Rechercher tous les fichiers d'aide...:ehp2." du menu
":hp2.Outils:ehp2." ou via la combinaison de touches
:hp2.Ctrl:ehp2.&plus.:hp2.S:ehp2.&per.
:p.Cette recherche fonctionne de la m�me mani�re que la
:link reftype=hd refid='Recherche'.recherche normale:elink.&comma. mais
elle vous dit en plus dans quel fichier d'aide les r�sultats ont �t�
trouv�s.
:p.Ces recherches peuvent prendre du temps en fonction de ce que vous
cherchez. Vous pouvez l'interrompre � n'importe quel moment.
:p.:hp2.O� :ehp2.&prodname.:hp2. cherche-t-il  ? :ehp2.
:p.Par d�faut, la recherche des fichiers d'aide s'effectue dans les
chemins de recherche sp�cifi�s dans les
:link reftype=hd refid='VariablesEnvironnement'.variables
d'environnement:elink. :hp2.BOOKSHELF:ehp2. et :hp2.HELP:ehp2.&per.
Vous pouvez choisir d'autres emplacements depuis la liste de s�lection
d�roulante, ou personnaliser la liste en cliquant sur le bouton
:hp2.S�lection...:ehp2.&per.
:p.:hp2.Recherche dans &colon. chemins d'aide standards:ehp2.
C'est l'�tendue de recherche par d�faut, et se fera selon les chemins
:hp2.BOOKSHELF:ehp2. et :hp2.HELP:ehp2. cit�s plus haut.
:p.En cliquant sur le bouton :hp2.S�lection...:ehp2.&comma. vous aurez la
possibilit� de choisir quels seront les r�pertoires des chemins de
recherche qui seront � utiliser. Cliquez sur chaque �l�ment de la liste
pour le s�lectionner ou le d�s�lectionner. Apr�s avoir fait votre
s�lection, l'emplacement s'affichera comme :hp2.Chemins d'aide
s�lectionn�s:ehp2.&per.
:p.:hp2.Recherche dans &colon. toutes les unit�s de disques:ehp2.
:p.Cette option permet d'effectuer une recherche sur toutes les unit�s de
disques durs (non amovibles) de votre syst�me. Vous pouvez l� encore
cliquer sur le bouton :hp2.S�lection...:ehp2. pour faire votre s�lection.
:p.La recherche dans les unit�s permet certes de toucher tous les
fichiers d'aide, mais se r�v�le bien plus lente qu'une recherche uniqement
dans les chemins d'aide.
:p.:hp2.Recherche dans &colon. chemins d'aide s�lectionn�s:ehp2.
:p.Si vous avez d�j� s�lectionn� des chemins d'aide particuliers pour la
recherche, vous pouvez cliquer sur le bouton :hp2.S�lection...:ehp2. pour
personnaliser � nouveau cette s�lection.
:p.:hp2.Rechercher dans &colon. liste de r�pertoires:ehp2.
:p.Dans la bo�te de dialogue :hp2.S�lection de r�pertoires de
recherche:ehp2.&comma. cliquer sur le bouton :hp2.Ajouter...:ehp2. vous
permettra d'ajouter un ou plusieurs r�pertoire � la liste de recherche.
:p.S�lectionnez l'unit� et le r�pertoire en utilisant la bo�te de
contr�le qui appara�t, puis cliquez sur le bouton :hp2.&lt. Ajouter:ehp2.
pour ajouter � la liste le r�pertoire en cours de s�lection. Activez
l'option :hp2.avec les sous-r�pertoires:ehp2. si vous voulez que les
sous-r�pertoire du r�pertoire en cours de s�lection soient aussi ajout�s
� la liste de recherche. Dans ce cas, ":hp2....:ehp2." s'affichera � la
fin du r�pertoire.
:p.Apr�s avoir ainsi ajout� un dossier personnalis�, l'emplacement de
recherche affichera :hp2.liste de r�pertoires:ehp2.
:note.si vous ajoutez un r�pertoire personnalis� aux chemins d'aide
standards ou aux chemins d'aide s�lectionn�s, la liste deviendra alors
une liste personnalis�e, et vous ne pourrez pas res�lectionner les
chemins d'aide. Pour retourner aux chemins d'aide d'origine,
s�lectionnez :hp2.chemins d'aide standards:ehp2. et cliquez sur le
bouton :hp2.S�lection...:ehp2. � nouveau.
:p.:hp2.Rechercher dans &colon. entrer un emplacement:ehp2.
:p.Vous pouvez taper une unit� ou un r�pertoire dans le champ d'entr�e
:hp2.Rechercher dans:ehp2.&per. Ajoutez ":hp2....:ehp2." � la fin du
r�pertoire si vous voulez inclure aussi les sous-r�pertoires dans la
recherche.
:p.Exemple &colon.
:p.:hp2.Rechercher dans &colon. [E&colon.\MesDocuments\...]:ehp2.
:p.Lancera la recherche dans les fichiers d'aide dans
E&colon.\MesDocuments\ et ses sous-r�pertoires.
:p.
.**************************************************************************
.* SECTION : BOOKMARKS                                                    *
.**************************************************************************
:h1 res=8 id='Signets'.Signets
:p.:hp2.Signets:ehp2.
:p.&prodname. vous permet de mettre en signet une section particuli�re du
fichier d'aide en cours. Cliquez simplement sur le bouton
":hp2.Signet:ehp2." :artwork runin name='..\images\bookmark.bmp'. pour ajouter la
section en cours � la liste des signets.
:p.Pour vous rendre � un signet, allez dans le menu ":hp2.Signets:ehp2."
et cliquez sur celui que vous voulez ouvrir.
:p.Vous pouvez voir ou supprimer tous vos signets en cliquant sur l'option
":hp2.�diter...:ehp2." du menu ":hp2.Signets:ehp2.". Cette fen�tre peut
rester ouverte pendant la consultation du fichier d'aide, de mani�re �
garder la liste des signets sous les yeux.
:p.
:note text='Remarques &colon.'.
:p.les signets de &prodname. se "souviennent" de toutes les fen�tres de
sections ouvertes s'il y en a plus d'une.
:p.Les signets sont sauvegard�s dans un fichier portant l'extension
&per.BMK, situ� dans le m�me r�pertoire que le fichier d'aide auquel ils
se rapportent.
:p.
.**************************************************************************
.* SECTION : INTERNET LINKS                                               *
.**************************************************************************
:h1 res=100 id='LiensInternet'.Liens vers Internet
:p.:hp2.Liens vers Internet:ehp2.
:p.Lorsque vous cliquez sur un lien vers une ressource Internet ("URL" ou
"Uniform Resource Locator") comme
http&colon.&slash.&slash.www&per.google&per.fr, &prodname. lance
votre navigateur par d�faut.
:p.Ce navigateur est indiqu� dans les param�tres du syst�me
d'exploitation, pas dans &prodname. lui-m�me. Pour le configurer, vous
pouvez ouvrir un objet URL sur le bureau, �ditez le chemin et le nom
de fichier pour le navigateur dans l'onglet ":hp2.Afficheur:ehp2.", puis
cliquez sur le bouton ":hp2.Nouveau param�trage:ehp2.". Autrement, vous
pouvez t�l�charger l'utilitaire ConfigApps depuis Hobbes &colon.
http&colon.&slash.&slash.hobbes&per.nmsu&per.edu&slash.cgi-bin&slash.h-search?key=configapps
:p.Certains navigateurs peuvent aussi proposer de se param�trer comme
navigateur par d�faut soit � l'installation, soit dans les pr�f�rences.
:p.Les liens M�l, FTP et vers les fora de discussion ("newsgroups") sont
aussi pass�s au navigateur. � l'heure actuelle, il n'est pas possible de
choisir un programme diff�rent pour chacun de ces liens.
:p.:hp2.Remarques pour les auteurs de fichiers d'aide:ehp2.
:p.Le programme View original ne "comprends" absolument rien aux URL ou
liens M�l. La seule fa�on de les impl�menter �tait de faire un appel �,
par exemple, "NETSCAPE.EXE" avec les param�tres adequats.
:p.&prodname. traduit les liens vers les programmes "NETSCAPE", "EXPLORE"
ou "MOZILLA" en un lien vers le navigateur par d�faut.
:p.Il autod�tecte en outre les URL sous la forme &colon.
:ul compact.
:li.http&colon.&slash.&slash.x  https&colon.&slash.&slash.x
 ftp&colon.&slash.&slash.x
:li.mailto&colon.x  news&colon.x
:eul.
:p.Tout ce qui ressemble � un URL est aussi d�tect�, m�me sans le
pr�fixe de protocole &colon.
:ul compact.
:li.www&per.a&per.b &endash. Navigateur
:li.ftp&per.a&per.b &endash. lien ftp
:li.a&atsign.b&per.c &endash. lien M�l
:eul.
:p.O� a, b et c sont des cha�nes alphanum�riques quelconques.
:p.Vous n'avez rien besoin de faire pour que &prodname. les reconnaisse.
:p.
.**************************************************************************
.* SECTION : COMMANDLINE                                                  *
.**************************************************************************
:h1 res=9 id='LigneDeCommandes'.Param�tres de ligne de commandes
:p.:hp2.Param�tres de ligne de commandes:ehp2.
:p.Lorsque vous lancez &prodname. depuis la ligne de commandes, vous
pouvez lui fournir divers arguments. Aucun d'entre eux n'est requis.
:p.
:p.:hp2.:font facename='Courier' size=14x8.NewView &lbracket.options&rbracket. &lbracket.&lt.NomDeFichier&gt. &lbracket.&lt.Texte � rechercher&gt.&rbracket.&rbracket.:font facename='Default' size=0x0.:ehp2.
:p.Si &prodname. est install� en remplacement de View, la commande d�bute
alors par :hp2.VIEW:ehp2. au lieu de :hp2.NEWVIEW:ehp2.&per.
:p.:link reftype=hd refid='ExempleLigneDeCde'.Exemples:elink.
:ul.
:li.:hp2.&lt.NomDeFichier&gt.:ehp2.
.br
Le fichier que &prodname. doit charger. Vous pouvez charger plusieurs
fichiers simultan�ment en s�parant leurs noms par un
":hp2.&plus.:ehp2." &colon.
:hp2.NomDeFichier1&plus.NomDeFichier2&plus.&per.&per.&per.:ehp2.
:p.Si le chemin n'est pas indiqu�, les fichiers seront alors recherch�s
dans les chemins figurant dans les
:link reftype=hd refid='VariablesEnvironnement'.variables d'environnement
BOOKSHELF et HELP:elink.&per.
:li.:hp2.&lt.Texte � rechercher&gt.:ehp2.
.br
Recherche le texte donn� dans les titres des sections et les entr�es
d'index. Cela :hp2.n:ehp2.'agit :hp2.pas:ehp2. comme une recherche
normale, pour garder la compatibilit� avec View. Pour effectuer la
recherche appropri�e, utilisez l'option &slash.s &lpar.voir
ci-dessous&rpar.&per. Veuillez consulter
:link reftype=hd refid='RechSectionLigneDeCde'.Recherche de section en
ligne de commandes:elink.&per.
:eul.
:p.:hp2.Options:ehp2.
:ul.
:li.:hp2.&slash.s:ehp2.
.br
Apr�s l'ouverture du fichier, une
:link reftype=hd refid='Recherche'.recherche:elink. normale du texte
donn� est effectu�e.
.br
Exemple &colon.
:ul compact.
:li. pour rechercher le mot "copie" dans tout le document cmdref, vous
pouvez utiliser &colon.
.br
:hp2.:font facename='Courier' size=14x8.NewView &slash.s cmdref copie:font facename='Default' size=0x0.:ehp2.
:li.&prodname. est assez intellignet pour prendre en compte plusieurs
mots. Comme dans le
:link reftype=hd refid='Recherche'.panneau de navigation de recherche:elink.&comma.
il s'agit d'une recherche "OR" &colon.
Passez un nom de fichier d'aide particulier comme premier param�tre
si vous voulez qu'il soit ouvert avant que la recherche ne soit
lanc�e &colon.
.br
:hp2.:font facename='Courier' size=14x8.NewView &slash.s cmdref net access:font facename='Default' size=0x0.:ehp2.
:li.Pour effectuer une recherche "AND", placez la phrase � rechercher
entre guillemets &colon.
;br
:hp2.:font facename='Courier' size=14x8.NewView &slash.s cmdref "net access":font facename='Default' size=0x0.:ehp2.
:eul.
:li.:hp2.&slash.g:ehp2.
.br
Effectue une
:link reftype=hd refid='RechercheGlobale'.recherche globale:elink. pour
le texte donn� dans tous les fichiers d'aide de votre syst�me.
.br
Exemple &colon.
:ul compact.
:li. pour rechercher le mot "copie" dans tous les fichiers d'aide,
utilisez &colon.
.br
:hp2.:font facename='Courier' size=14x8.NewView &slash.g copie:font facename='Default' size=0x0.:ehp2.
:li.Passez un nom de fichier d'aide particulier comme premier param�tre
si vous voulez qu'il soit ouvert avant que la recherche ne soit
lanc�e &colon.
.br
:hp2.:font facename='Courier' size=14x8.NewView &slash.g cmdref copie:font facename='Default' size=0x0.:ehp2.
:eul.
:li.:hp2.&slash.?:ehp2.&comma. :hp2.&slash.h:ehp2. ou :hp2.&slash.help:ehp2.
.br
Affichage de l'aide en ligne de commandes.
:eul.
:p.Voir aussi &colon.
:ul compact.
:li.:link reftype=hd refid='ParametresAvances'.Param�tres avanc�s:elink.
:eul.
:p.
.**************************************************************************
.* SECTION : COMMANDLINETOPICSEARCH                                       *
.**************************************************************************
:h2 res=13 id='RechSectionLigneDeCde'.Recherche de section en ligne de commandes
:p.:hp2.Recherche de section en ligne de commandes:ehp2.
:p.:hp2.:font facename='Courier' size=14x8.view &lt.NomDeFichier&gt. &lt.Section&gt.:font facename='Default' size=0x0.:ehp2.
:p.Le param�tre de recherche de section indiqu� en ligne de commandes,
est calqu� sur le comportement de l'ancien programme View.
:p.Le texte � l'int�rieur des sections ne sera pas pris en compte, seuls
les titres et entr�es d'index le seront. Cette recherche est moins utile
pour les humains, mais elle est utilis�e par quelques programmes pour
r�f�rencer les sujets d'aide de mani�re pr�dictive.
:p.Vous pouvez utiliser ici plus d'un mot.
:p.La recherche se d�roule ici de la sorte &colon.
:ul compact.
:li.le titre de section d�bute par le texte recherch�,
:li.l'entr�e d'index d�bute par le texte recherch�,
:li.le titre de section contient le texte recherch�,
:li.l'entr�e d'index contient le texte recherch�.
:eul.
:p.Les d�veloppeurs devront s'assurer que le document attendu sera trouv�
si cette technique est utilis�e pour identifier des sections en lan�ant le
nouveau ou l'ancien afficheur.
:p.
.**************************************************************************
.* SECTION : ADVANCEDPARAMETERS                                           *
.**************************************************************************
:h2 res=14 id='ParametresAvances'.Param�tres avanc�s
:p.:hp2.Param�tres avanc�s:ehp2.
:p.Les param�tres de ligne de commandes suivants sont principalement
destin�s aux d�veloppeurs de logiciels, mais peuvent �tre utilis�s pour
d'autres buts.
:p.:hp2.:font facename='Courier' size=14x8.&slash.lang&colon.&lt.langue&gt.:font facename='Default' size=0x0.:ehp2.
:p.Charge la langue indiqu�e. Se substitue � la valeur par d�faut,
choisie en fonction de la variable d'environnement :hp2.LANG:ehp2.&per.
Par exemple,
:font facename='Courier' size=14x8.newview cmdref &slash.lang&colon.fr:font facename='Default' size=0x0.
charge le fran�ais. Veuillez vous r�f�rer au fichier
:hp2.LisezMoi.txt:ehp2. pour plus d'informations.
:p.:hp2.:font facename='Courier' size=14x8.&slash.pos&colon.&lt.Gauche&gt.&comma.&lt.Droite&gt.&comma.&lt.Largeur&gt.&comma.&lt.Hauteur&gt.:font facename='Default' size=0x0.:ehp2.
:p.Param�tre la fen�tre principale du programme aux position et taille
donn�es. Toutes les valeurs doivent �tre indiqu�es. Placez un
:hp2.P:ehp2. apr�s un nombre pour indquer un pourcentage.
Par exemple &colon.
.br
:font facename='Courier' size=14x8.newview &slash.pos&colon.10P&comma.10P&comma.80P&comma.80P:font facename='Default' size=0x0.
.br
donne une fen�tre centr�e dont la taille fait 80&percent.de celle de
l'�cran.
:p.:hp2.:font facename='Courier' size=14x8.&slash.title&colon.&lt.Titre de fen�tre&gt.:font facename='Default' size=0x0.:ehp2.
:p.Indique le titre de la fen�tre de &prodname. qui prendra le texte
donn�, se substitutant � ce qui doit appara�tre dans le fichier d'aide.
Le texte "Aide &endash.  " sera toujours ins�r� avant la cha�ne de
caract�res donn�e. � moins que celle-ci elle-m�me ne soit "Aide", auquel
cas, le titre deviendra simplement "Aide". Ceci pour �tre certain que les
fen�tres d'aide soient toujours reconnues comme telles dans la liste de
fen�tres.
:p.Si vous deviez indiquer plusieurs mots, encadrez l'option enti�re avec
des guillemets. Par exemple &colon.
.br
:hp2.:font facename='Courier' size=14x8.newview cmdref "&slash.title&colon.Introduction aux commandes OS&slash.2":font facename='Default' size=0x0.:ehp2.
:p.
.**************************************************************************
.* SECTION : COMMANDLINEEXAMPLES                                          *
.**************************************************************************
:h2 res=15 id='ExempleLigneDeCde'.Exemples de lignes de commandes
:p.:hp2.Exemples de lignes de commandes:ehp2.
:p.Les exemples suivants assument que &prodname. a �t� install� en
remplacement du programme View original, et que, par cons�quent, View est
en fait &prodname.&per.
:p.:hp2.:font facename='Courier' size=14x8.view cmdref:font facename='Default' size=0x0.:ehp2.
:p.Ouvre le fichier :hp2.CMDREF&per.INF:ehp2. &lpar.Manuel de r�f�rence
OS&slash.2 Warp&rpar. trouv� dans l'un des chemins sp�cifi�s dans la
variable d'environnement :hp2.HELP:ehp2.&per.
:p.:hp2.:font facename='Courier' size=14x8.view cmdref&plus.os2ug:font facename='Default' size=0x0.:ehp2.
:p.Ouvre les deux fichiers :hp2.CMDREF&per.INF:ehp2. et
:hp2.OS2UG&per.INF:ehp2. &lpar.OS&slash.2 Warp - Guide
d'utilisation&rpar. dans la m�me fen�tre.
:p.La table des mati�res de :hp2.OS2UG&per.INF:ehp2. est ajout�e � la fin
de celle de :hp2.CMDREF&per.INF:ehp2.&per. Les index sont combin�s
alphab�tiquement.
:p.:hp2.:font facename='Courier' size=14x8.view c&colon.&bsl.os2&bsl.book&bsl.os2ug&per.inf:font facename='Default' size=0x0.:ehp2.
:p.Ouvre le fichier :hp2.OS2UG&per.INF:ehp2. pris dans le r�pertoire
":hp2.c&colon.&bsl.os2&bsl.book:ehp2.".
:p.:hp2.:font facename='Courier' size=14x8.view "c&colon.&bsl.os2 book&bsl.os2ug&per.inf":font facename='Default' size=0x0.:ehp2.
:p.Encadrez le chemin d'acc�s au fichier par des guillemets s'il contient
des caract�res sp�ciaux (comme les espaces).
:p.:hp2.:font facename='Courier' size=14x8.view cmdref dir:font facename='Default' size=0x0.:ehp2.
:p.Ouvre le fichier :hp2.CMDREF:ehp2. &lpar.Manuel de r�f�rence OS&slash.2
Warp&rpar. et recherche le mot "dir" dans les titres et les index, pour
afficher la page d'aide de la commande DIR.
:p.:hp2.:font facename='Courier' size=14x8.view &slash.s&colon.os2ug bureau:font facename='Default' size=0x0.:ehp2.
:p.Ouvre le fichier :hp2.OS2UG&per.INF:ehp2. et recherche le mot
"Bureau". La meilleure correspondance sera affich�e.
:p.:hp2.:font facename='Courier' size=14x8.view &slash.g&colon.permissions:font facename='Default' size=0x0.:ehp2.
:p.Effectue une recherche du mot "permissions" dans tous les fichiers
d'aide.
:p.:hp2.:font facename='Courier' size=14x8.set mon_aide&eq.cmdref&plus.os2ug&plus.rexx:font facename='Default' size=0x0.:ehp2.
:p.:hp2.:font facename='Courier' size=14x8.view mon_aide:font facename='Default' size=0x0.:ehp2.
:p.La premi�re ligne d�finit une variable d'environnement
:hp2.MON_AIDE:ehp2. qui contient les noms de trois fichiers d'aide. La
seconde ligne ouvre ces trois fichiers.
:p.
.**************************************************************************
.* SECTION : KEYBOARDSHORTCUTS                                            *
.**************************************************************************
:h1 res=10 id='KeyboardShortcuts'.Touches de raccourcis
:p.:hp2.Touches de raccourcis:ehp2.
:p.La plupart des touches de raccourcis clavier sont affich�es dans les
menus, mais certaines ne le sont pas.
:p.:hp2.Les touches de raccourcis suppl�mentaires sont &colon.:ehp2.
:p.:hp2.Alt&plus.F4:ehp2. &endash. Sortie du programme
:p.:hp2.Ctrl&plus.C:ehp2. &endash. Copie du texte s�lectionn� dans le
presse-papiers
:p.:hp2.F7:ehp2. &endash. En arri�re
:p.:hp2.F8:ehp2. &endash. En avant
:p.:hp2.Ctrl&plus.Gauche:ehp2. &endash. En arri�re
:p.:hp2.F11:ehp2. &endash. Section pr�c�dente dans la table des mati�res
:p.:hp2.F12:ehp2. &endash. Section suivante dans la table des mati�res
:p.
:p.:hp2.Les touches de raccourcis visibles dans le menu &colon.:ehp2.
:p.:hp2.Ctrl&plus.O:ehp2. &endash. Ouverture de fichiers
:p.:hp2.Ctrl&plus.E:ehp2. &endash. Ouverture des fichiers dans les
chemins d'aide
:p.:hp2.Ctrl&plus.N:ehp2. &endash. Ouverture d'une nouvelle fen�tre
:p.:hp2.Ctrl&plus.P:ehp2. &endash. Impression de la section
:p.:hp2.F3:ehp2. &endash. Sortie du programme
:p.
:p.
:p.:hp2.Ctrl&plus.A:ehp2. &endash. S�lection de tout le texte dans la
section
:p.:hp2.Ctrl&plus.Inser:ehp2. &endash. Copie du texte s�lectionn� dans le
presse-papiers
:p.
:p.:hp2.Ctrl&plus.F:ehp2. &endash. Recherche dans la section en cours
:p.:hp2.Ctrl&plus.G:ehp2. &endash. Continuer la derni�re recherche
:p.
:p.:hp2.Ctrl&plus.S:ehp2. &endash. Ouverture de l'outil de recherche
globale
:p.
:p.:hp2.Alt&plus.C:ehp2. &endash. Ouverture de l'onglet Contenu
:p.:hp2.Alt&plus.I:ehp2. &endash. Ouverture de l'onglet Index
:p.:hp2.Alt&plus.S:ehp2. &endash. Ouverture de l'onglet Recherche
:p.:hp2.Alt&plus.N:ehp2. &endash. Ouverture de l'onglet Notes
:p.:hp2.Alt&plus.P:ehp2. &endash. Activation&slash.d�sactivation du
panneau de gauche &lpar.onglets&rpar.
:p.:hp2.F5:ehp2. &endash. D�roulement de toute l'arborescence du contenu
:p.:hp2.F6:ehp2. &endash. Enroulement de toute l'arborescence du contenu
:p.
:p.:hp2.�chap:ehp2. &endash. En arri�re
:p.:hp2.Ctrl&plus.Droite:ehp2. &endash. En avant
:p.:hp2.Ctrl&plus.Haut:ehp2. &endash. Section pr�c�dente dans la table des
mati�res
:p.:hp2.Ctrl&plus.Bas:ehp2. &endash. Section suivante dans la table des
mati�res
:p.
:p.:hp2.Ctrl&plus.D:ehp2. &endash. �dition des signets
:p.:hp2.Ctrl&plus.B:ehp2. &endash. Mise en signets de la section en cours
:p.
:p.:hp2.Ctrl&plus.M:ehp2. &endash. Ajout d'une note � la position du
curseur
:p.
:p.:hp2.F1:ehp2. &endash. Affichage de l'aide de &prodname.
:p.
.**************************************************************************
.* SECTION : L_ENVIRONMENTVARIABLES                                       *
.**************************************************************************
:h1 res=11 id='VariablesEnvironnement'.Variables d'environnement
:p.:hp2.Variables d'environnement:ehp2.
:p.
:p.Les deux variables d'environnement :hp2.BOOKSHELF:ehp2. et
:hp2.HELP:ehp2. d�finissent les chemins &lpar.liste de r�pertoires&rpar.
pour la recherche de fichiers d'aide. &prodname. les utilise sans
distinction.
:p.Ces chemins sont utilis�s lorsque vous &colon.
:ul compact.
:li.sp�cifiez un fichier d'aide sans chemin d'acc�s comme argument en
ligne de commandes,
:li.utilisez l'option ":hp2.Ouvrir avec param�tres...:ehp2." du menu
":hp2.Fichier:ehp2.",
:li.effectuez une :link reftype=hd refid='RechercheGlobale'.recherche
globale:elink.&per.
:eul.
:p.Vous pouvez ajouter des r�pertoires de fichiers d'aide de fa�on
permanente aux chemins des variables d'environnement :hp2.HELP:ehp2. ou
:hp2.BOOKSHELF:ehp2. en modifiant le fichier
:hp2.CONFIG&per.SYS:ehp2.&per. Ajoutez-les aux deux si vous voulez aussi
que l'ancien programme View soit capable de trouver les fichiers.
:p.:hp2.Autres variables d'environnement:ehp2.
:p.La variable d'environnement :hp2.LANG:ehp2. est analys�e pour d�cider
quelle la langue par d�faut &prodname. doit adopter (red�finie par le
:link reftype=hd refid='ParametresAvances'.param�tre de ligne de
commandes &slash.lang:elink.&rpar.&per. Veuillez consulter le fichier
:hp2.LisezMoi.txt:ehp2. de &prodname. pour plus d'informations sur le
support multilingue.
:p.Le r�pertoire d�fini dans :hp2.LOGFILES:ehp2. est utilis� pour les
rapports d'erreurs (de plantages) ou d'autres informations.
:p.Le sous-r�pertoire "lang" du r�pertoire d�fini par :hp2.OSDIR:ehp2. est
utilis� pour la recherche de fichiers de langues au d�marrage.
:p.Le chemin :hp2.ULSPATH:ehp2. est lui aussi utilis� pour la recherche
des fichiers de langue.
:p.
.**************************************************************************
.* SECTION : FORAUTHORS                                                   *
.**************************************************************************
:h1 res=20 id='PourLesAuteurs'.Notes pour les auteurs et d�veloppeurs
:p.:hp2.Notes pour les auteurs et d�veloppeurs:ehp2.
:p.
:p.Cette section rassemble quelques informations destin�es aux auteurs et
d�veloppeurs de logiciels.
:p.Voir aussi la section traitant de la reconnaissance des URL &colon.
:ul compact.
:li.:link reftype=hd refid='LiensInternet'.Liens vers internet:elink.&per.
:eul.
:p.
.**************************************************************************
.* SECTION : WRITINGHELPFILES                                             *
.**************************************************************************
:h2 res=12 id='CreerFichierAide'.Cr�ation de fichiers d'aide
:p.:hp2.Cr�ation de fichiers d'aide OS&slash.2:ehp2.
:p.
:p.Les fichiers d'aide d'OS&slash.2 sont cr��s � l'aide d'un compilateur
d'IPF &colon. IFP Compiler. IPF signifie "Information Presentation
Facility" (service pour la pr�sentation de l'information).
:p.Le comp�lateur IPF prend en entr�e un fichier texte �crit dans un
langage prenant en compte des tas de choses comme les ent�tes, les liens,
le texte et les images, et produit - en sortie, donc - un fichier &per.INF
ou &per.HLP.
:p.La mani�re officielle d'obtenir le compilateur IPF
&lpar.IPFC&per.EXE&rpar. est d'acqu�rir le Kit de d�veloppement pour
OS&slash.2 (ou "OS&slash.2 Developers Toolkit"). Il est fourni
gratuitement avec eComStation
&lpar.http&colon.&slash.&slash.www&per.ecomstation&per.com&rpar.&per.
:p.Comme le langage pour IPFC est relativement p�nible &lpar.la
ponctuation, par exemple, devrait �tre remplac�e par des mots-cl�s
sp�ciaux comme "&amp.comma&per." pour la virgule&rpar.&comma. pas mal de
personnes utilisent des outils en plus du compilateur IPF lui-m�me.
:p.Un outil graphique et simple qu'on peut utiliser est Vyperhelp
&lpar.http&colon.&slash.&slash.www&per.vyperhelp&per.com&rpar.&per.
Il peut en outre g�n�rer de l'aide Windows, du HTML et autres, bien qu'il
ne tourne que sous OS&slash.2 (ou eComStation). Il n'est pas gratuit.
:p.Les autres options &colon.
:ul compact.
:li.Le pr�processeur IPF HyperText&slash.2
&lpar.http&colon.&slash.&slash.www&per.clanganke&per.de&slash.os2&slash.sw&slash.htext&slash.&rpar.
pr�pare et g�n�re du format IPF plus compliqu� � partir d'un langage plus
simple. Gratuit.
:li.HyperMake &lpar.http&colon.&slash.&slash.www&per.hypermake&per.com&rpar.
agit de la m�me mani�re que le pr�c�dent, mais peut g�n�rer de l'aide
Windows et du HTML.
:li.Sibyl &lpar.� l'aide duquel &prodname. a �t� cr��&rpar. fournit un
pr�processeur IPF.
:li.IPFEditor de PCS
&lpar.http&colon.&slash.&slash.www&per.pcs&endash.soft&per.com&slash.productipfe212&per.htm&rpar.
probablement le plus complet, mais avec un co�t significatif.
:warning.&prodname. ne prend pas en charge tout ce que IPFE peut faire &xclm.
:eul.
:p.Dans le pass�, il y avait d'autres solutions. Celles list�es devraient
toujours �tre disponibles et �tre support�es.
:p.
.**************************************************************************
.* SECTION : TOPICRESOURCEIDS                                             *
.**************************************************************************
:h2 res=16 id='IDRessourceSection'.Identifiants de ressource des sections
:p.:hp2.Identifiants de ressource des sections:ehp2.
:p.Identifiants de ressource &lpar."Resource IDs"&rpar. sont utilis�s par
les auteurs pour l'aide en ligne des applications, pour identifier une
section d'aide. Les applications appellent le gestionnaire d'aide en
sp�cifiant un identifiant de ressource, soit directement, soit
indirectement via des tables d'aide ajout�es aux ressources, ce que le
Gestionnaire de Pr�sentation &lpar."Presentation Manager" ou "PM"&rpar.
g�re automatiquement. Les identifiants de ressource sont stock�s dans
une table � l'int�rieur du fichier d'aide.
:p.Aux auteurs de documents, &prodname. offre la possibilit� de voir
et rechercher les identifiants de ressource.
:p.:hp2.Recherche par identifiants de ressource:ehp2.
:p.Utiliser l'option ":hp2.Rechercher section par son ID:ehp2." du
menu ":hp2.Outils:ehp2." pour rechercher un identifiant de ressource
indiqu� dans tous les fichiers ouverts.
:p.:hp2.Affichage des identifiants de ressource:ehp2.
:p.Ouvrez les propri�t�s de la section &lpar.clic sur le bouton droit de
la souris sur le contenu de la section en cours &endash.
":hp2.Propri�t�s:ehp2."&rpar. pour voir le(s) identifiant(s) de ressource
associ�(s) � cette section.
:p.
.**************************************************************************
.* SECTION : TOPICRESOURCEIDS                                             *
.**************************************************************************
:h2 res=19 id='NomsSection'.Noms de sections
:p.:hp2.Noms de sections:ehp2.
:p.
:p.Comme les identifiants de ressource, les noms de section peuvent �tre
utilis�s par les d�veloppeurs pour faire des liens vers les sections
d'aide depuis leur application, en utilisant le HM&us.DISPLAY&us.HELP avec
HM&us.PANELNAME param�tre 2.
:p.Ils ne sont pas utilis�s si souvent.
:p.&prodname. peut trouver un nom de section particulier en utilisant
l'option ":hp2.Rechercher section par son nom:ehp2." du menu
":hp2.Outils:ehp2."&per.
:p.
:euserdoc.
