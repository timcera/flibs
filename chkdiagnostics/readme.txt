Opties van Ondrej Certic:
-Wall -Wextra -Wimplicit-interface -Wno-unused-function -fPIC -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999

The -finit-derived is a good one and should be used also, I didn’t know about it. The -ffpe-trap are important to stop a program when a NaN is generated, and the -fbacktrace is important to give you a nice stacktrace.

Toevoegen: -fimplicit-none -std=f2008


Wat voor opties met Intel Fortran?

-warn:all geeft er al veel

-check:uninit
-check:bounds

-stand:f15

Een set programma's opzetten à la chkfeatures?

Elk programma moet aangeven wat voor diagnostische meldingen er
idealiter zouden zijn


3)
routine aangeroepen met verschillende argumentenlijsten

4)
routine met als argument x en x(1) - toegestaan volgens F77 (in ieder
geval veel gebruikt)

5)
externe functie f, geen interface, met argument real x - dus herkenbaar
als functie

6)
uitdrukkingen met gemengde precisie

7)
constanten met te veel precisie, maar geen "kind": real :: pi = 3.14159265358979323846

8)
variabelen uit de host die in een contained routine worden gebruikt (al
dan niet per ongeluk)

9)
incorrecte formats:
- verkeerde edit (J10 ipv I10)
- ontbrekende haakjes
- verkeerd type variabele

10)
rechtstreeks vergelijken real variabelen

11)
waarschuwingen over tijdelijke arrays: call sub( x(1:1000:10) )

12)
ongebruikte variabelen: lokaal in een programma-eenheid, argumenten van
een routine

13)
verwijderde aspecten:
- do real-variabele
- pause
- holleriths
- assigned goto
- goto naar einde if-branch

14)
kaal SAVE-statement - niet verboden of verkeerd, maar wel iets wat je
eigenlijk niet moet gebruiken

15)
routine die zeker een interface nodig heeft, zichtbaar voor de compiler
(bijvoorbeeld in dezelfde file maar niet in een module)

16)
functies die geen waarde doorgeven - wellicht alleen via bepaalde paden

17)
subroutines met intent(out) argumenten die niet gezet worden

18)
subroutines en functies die argumenten niet gebruiken
