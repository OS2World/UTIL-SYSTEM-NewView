.**********************************************************
.*
.* NewView Rendering Tests
.*
.* RBRi 2006
.*
.**********************************************************
.*
:userdoc.
:docprof toc=12 ctrlarea=none.
:title.NewView Rendering Tests
.*
.*
.**********************************************************
:h1.Overview
:p.This is a test document for NewView. Every page contains
some samples/tests for a specific problem
.*
.*
.**********************************************************
:h1.Tests
:p.
Tests
.*
.*
.**********************************************************
:h2.Italic text
:p.
This page contains various samples to sort out problems with
highlighted (italic) text.
:lm margin=4.
:p.
The space between the highlighted word and the next one is to short.
.br
:lm margin=10.Normal text :hp1.highlighted:ehp1. and again normal.
.br
:hp6.Max speed :ehp6.turns (there is a space between speed and turns)
.*
:lm margin=4.
:p.
Some samples for display problems when a short normal character follows
a highlighted one
.br
:lm margin=10.Normal text :hp1.Hint:ehp1.&colon. and again normal (colon is normal).
.br
Normal text :hp1.MLE:ehp1.. and again normal (dot is normal)
.*
.*
.**********************************************************
:h2.Spacing with an non proportional font
Spacing with an non proportional font
:font facename=Courier size=13x8.
.*
:lm margin=4.
:p.
The space between the highlighted word and the next one is to short.
.br
:lm margin=10.Normal text :hp1.highlighted:ehp1. and again normal.
.br
:hp6.Max speed :ehp6.turns (there is a space between speed and turns)
.*
:lm margin=4.
:p.
Some samples for display problems when a short normal character follows
a highlighted one
.br
:lm margin=10.Normal text :hp1.Hint:ehp1.&colon. and again normal (colon is normal).
.br
Normal text :hp1.MLE:ehp1.. and again normal (dot is normal)
.*
:font facename=default size=0x0.
:lm margin=1.
:p.
Character graphic
:lm margin=4.
:cgraphic.
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³:hp1.File:ehp1.   Edit  ³ View ³ Options   Help ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÙ
              ³ All                ³
              ³ Some . . .         ³
              ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
              ³ By . . .           ³
              ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.
.*
.*
.**********************************************************
:h2.Blank at the beginning
:artwork name='fm3.bmp' align=center.
There is a blank at the begining of the first line after an artwork.
.br
Normal beginning....
.*
.*
.**********************************************************
:h2.Blank at the beginning (II)

There is a blank at the begining if in the source is an empty line after the heading.
.br
Normal beginning....
.*
.*
.**********************************************************
:h2.Problem with underscores in search string

Please search with the sting 'test_' and select this topic form the list of founded pages.
.br
Then have a look at the selections...
:p.
normal test_ blahblah
:p.
something different _ with an underscore
:p.
something with error in this line
.*
.*
.**********************************************************
:h2.Text Color
:p.The color handling is separated from the text formating.
The color shoud not stop after a formating like 'highlighted'
ends.
:p.
Normal text :hp1.highlighted:ehp1. and again normal.
.br
:color fc=red.
Normal red text :hp1.highlighted:ehp1. and again normal.
.br
Color only stops if :color fc=green. another colors starts or
:h3.heading follows
with new text
.*
.*
.**********************************************************
:h2.Text Color II
:p.Now text with a :color fc=default bc=palegray.F1:color fc=default bc=default.
 and the rest of the sentence.
.*
.*
.**********************************************************
:h2.Text For"mating
:p.
Test
.*
.*
.**********************************************************
:h2.Link type 'launch'
:p.This kind of links are used to start an external program.
There are serveral test to launch view.exe.
:ol.
:li.:link reftype=launch object='view.exe' data='test_topics.hlp TestTopic'.
object='view.exe' data='test_topics.hlp TestTopic'
:elink.
:li.NewView (and also the original view) is not able to handle the call of
executables with blanks in file/path name correct.
:link reftype=launch object='c:\os 2\e.exe' data=''.
object='c:\os 2\e.exe' data=''
:elink.
:eol.
.*
.*
.**********************************************************

.*
.*
.**********************************************************
:euserdoc.
.*
.**********************************************************
