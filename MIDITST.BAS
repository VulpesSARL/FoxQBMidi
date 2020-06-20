DECLARE SUB ShowHelp ()
DECLARE FUNCTION IIF% (condition!, true!, false!)
DECLARE SUB LoadInstrument ()
DECLARE SUB WriteDecayAlgFeedback (Alg AS INTEGER, FB AS INTEGER)
DECLARE SUB WriteADSR2 (i AS INTEGER, a AS INTEGER, D AS INTEGER, S AS INTEGER, r AS INTEGER, W AS INTEGER, M AS INTEGER, T AS INTEGER, SK AS INTEGER, Vib AS INTEGER, AM AS INTEGER, KSL AS INTEGER)
DECLARE SUB WriteADSR (O AS INTEGER, a AS INTEGER, D AS INTEGER, S AS INTEGER, r AS INTEGER, W AS INTEGER, M AS INTEGER, T AS INTEGER, SK AS INTEGER, Vib AS INTEGER, AM AS INTEGER, KSL AS INTEGER, Alg AS INTEGER, FB AS INTEGER)
DECLARE SUB PlayNote (Channel AS INTEGER, Note AS INTEGER)
DECLARE SUB WriteAdlib (Reg AS INTEGER, Value AS INTEGER)
DECLARE SUB DrawScroller (Text AS STRING, X AS INTEGER, colour AS INTEGER, Value AS INTEGER, max AS INTEGER)
DECLARE SUB DrawScreen (k AS STRING)

DIM SHARED Wave(0 TO 1) AS INTEGER
DIM SHARED Attack(0 TO 1) AS INTEGER
DIM SHARED Decay(0 TO 1) AS INTEGER
DIM SHARED Sustain(0 TO 1) AS INTEGER
DIM SHARED Release(0 TO 1) AS INTEGER
DIM SHARED Multi(0 TO 1) AS INTEGER
DIM SHARED Total(0 TO 1)  AS INTEGER
DIM SHARED SustainKeep(0 TO 1) AS INTEGER
DIM SHARED Vibrato(0 TO 1) AS INTEGER
DIM SHARED AM(0 TO 1) AS INTEGER
DIM SHARED KSL(0 TO 1)  AS INTEGER
DIM SHARED DecayAlg AS INTEGER
DIM SHARED FeedBack AS INTEGER
DIM SHARED LHPiano AS INTEGER

DIM SHARED SelectedItem AS INTEGER
DIM SHARED Operator AS INTEGER
DIM SHARED RedrawScreen AS INTEGER
DIM SHARED OPL2LPT  AS INTEGER

DIM SHARED ListI AS INTEGER
DIM SHARED ListS AS INTEGER

IF LCASE$(COMMAND$) = "opl2lpt" THEN OPL2LPT = 1

PRINT "Loading ..."

SelectedItem = 0
Operator = 0

Wave(0) = 0
Attack(0) = 15
Decay(0) = 2
Sustain(0) = 15
Release(0) = 4
Multi(0) = 1
Total(0) = 15
KSL(0) = 2

Wave(1) = 0
Attack(1) = 15
Decay(1) = 2
Sustain(1) = 15
Release(1) = 7
Multi(1) = 1
Total(1) = 6
KSL(1) = 0

FeedBack = 4
LHPiano = 0

DO
a = INP(&H331)
IF (a AND &H40) = 0 THEN
        OUT &H331, &HFF
        EXIT DO
END IF
LOOP

DO
a = INP(&H331)
IF (a AND &H40) = 0 THEN
        OUT &H331, &H3F
        EXIT DO
END IF
LOOP

FOR i% = 0 TO 256
WriteAdlib i%, 0
NEXT i%
WriteAdlib 1, &H20 'Enable Wave Select
PRINT "Init OK"
WriteADSR 0, Attack(0), Decay(0), Sustain(0), Release(0), Wave(0), Multi(0), Total(0), SustainKeep(0), Vibrato(0), AM(0), KSL(0), DecayAlg, FeedBack
WriteADSR 1, Attack(1), Decay(1), Sustain(1), Release(1), Wave(1), Multi(1), Total(1), SustainKeep(1), Vibrato(1), AM(1), KSL(1), DecayAlg, FeedBack
CLS

DIM Keys(0 TO 9) AS INTEGER
DIM Mode AS INTEGER
DIM Channel AS INTEGER
DIM Buffer(0 TO 2) AS INTEGER
DrawScreen ""
FOR i = 0 TO 9
Keys(i) = -1
NEXT i

DO
        DO
        a = INP(&H331)
        IF (a AND &H80) = 0 THEN
                M = INP(&H330)
        ELSE
                M = -1
        END IF
        key$ = INKEY$
        IF M <> -1 THEN
        MTST = M AND &H80
        IF MTST > 0 AND Mode > 0 THEN
        Mode = 0
        END IF
        IF Mode = 1 THEN 'Note off
                Buffer(0) = M
                Mode = 5
        ELSEIF Mode = 5 THEN 'Note off
                FOR i = 0 TO 9
                        IF Keys(i) = Buffer(0) THEN
                                PlayNote CINT(i), -1
                                Keys(i) = -1
                        END IF
                NEXT i
                Mode = 1
        ELSEIF Mode = 6 THEN 'Note on
                IF M > &HF THEN
                FOR i = 0 TO 9
                        IF Keys(i) = -1 THEN
                                PlayNote CINT(i), Buffer(0)
                                Keys(i) = Buffer(0)
                                EXIT FOR
                        END IF
                NEXT i
                ELSE
                FOR i = 0 TO 9
                        IF Keys(i) = Buffer(0) THEN
                                PlayNote CINT(i), -1
                                Keys(i) = -1
                        END IF
                NEXT i
                END IF
                Mode = 2
        ELSEIF Mode = 2 THEN 'Note on
                Buffer(0) = M
                Mode = 6
        ELSEIF Mode = 0 THEN
                IF M AND &H80 > 0 THEN
                     IF Channel = (M AND &HF) THEN
                         command = (M AND &HF0) / 16
                         IF command = &H8 THEN Mode = 1'Note off
                         IF command = &H9 THEN Mode = 2'Note on
                     END IF
                END IF
        END IF
        END IF
        IF RedrawScreen = 1 THEN
        key$ = ""
        RedrawScreen = 0
        EXIT DO
        END IF
        LOOP WHILE key$ = ""
        DrawScreen key$

IF key$ = CHR$(27) THEN EXIT DO
LOOP

FOR i% = 0 TO 256
WriteAdlib i%, 0
NEXT i%
LOCATE 25, 1

SUB DrawScreen (k AS STRING)
cmd$ = ""
IF LEN(k) = 2 THEN
IF ASC(MID$(k, 1, 1)) = 0 THEN
SELECT CASE ASC(MID$(k, 2, 1))
CASE &H48 'up
cmd$ = "u"
CASE &H4B 'left
cmd$ = "l"
CASE &H4D 'right
cmd$ = "r"
CASE &H50 'down
cmd$ = "d"
CASE &H49 'page up
cmd$ = "-"
CASE &H51 'page down
cmd$ = "+"
CASE &H3B 'F1
cmd$ = "help"
END SELECT
END IF
END IF

MaxSelected = 8

IF cmd$ = "l" THEN
SelectedItem = SelectedItem - 1
IF SelectedItem < 0 THEN SelectedItem = 0
END IF

IF cmd$ = "r" THEN
SelectedItem = SelectedItem + 1
IF SelectedItem > MaxSelected THEN SelectedItem = MaxSelected
END IF

IF cmd$ = "+" THEN
Operator = Operator + 1
IF Operator > 1 THEN Operator = 1
END IF

IF cmd$ = "-" THEN
Operator = Operator - 1
IF Operator < 0 THEN Operator = 0
END IF

IF cmd$ = "u" AND SelectedItem = 0 THEN
Wave(Operator) = Wave(Operator) + 1
IF Wave(Operator) > 3 THEN Wave(Operator) = 3
END IF
IF cmd$ = "d" AND SelectedItem = 0 THEN
Wave(Operator) = Wave(Operator) - 1
IF Wave(Operator) < 0 THEN Wave(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 1 THEN
Attack(Operator) = Attack(Operator) + 1
IF Attack(Operator) > 15 THEN Attack(Operator) = 15
END IF
IF cmd$ = "d" AND SelectedItem = 1 THEN
Attack(Operator) = Attack(Operator) - 1
IF Attack(Operator) < 0 THEN Attack(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 2 THEN
Decay(Operator) = Decay(Operator) + 1
IF Decay(Operator) > 15 THEN Decay(Operator) = 15
END IF
IF cmd$ = "d" AND SelectedItem = 2 THEN
Decay(Operator) = Decay(Operator) - 1
IF Decay(Operator) < 0 THEN Decay(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 3 THEN
Sustain(Operator) = Sustain(Operator) + 1
IF Sustain(Operator) > 15 THEN Sustain(Operator) = 15
END IF
IF cmd$ = "d" AND SelectedItem = 3 THEN
Sustain(Operator) = Sustain(Operator) - 1
IF Sustain(Operator) < 0 THEN Sustain(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 4 THEN
Release(Operator) = Release(Operator) + 1
IF Release(Operator) > 15 THEN Release(Operator) = 15
END IF
IF cmd$ = "d" AND SelectedItem = 4 THEN
Release(Operator) = Release(Operator) - 1
IF Release(Operator) < 0 THEN Release(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 5 THEN
Multi(Operator) = Multi(Operator) + 1
IF Multi(Operator) > 15 THEN Multi(Operator) = 15
END IF
IF cmd$ = "d" AND SelectedItem = 5 THEN
Multi(Operator) = Multi(Operator) - 1
IF Multi(Operator) < 0 THEN Multi(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 6 THEN
Total(Operator) = Total(Operator) + 1
IF Total(Operator) > 63 THEN Total(Operator) = 63
END IF
IF cmd$ = "d" AND SelectedItem = 6 THEN
Total(Operator) = Total(Operator) - 1
IF Total(Operator) < 0 THEN Total(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 7 THEN
KSL(Operator) = KSL(Operator) + 1
IF KSL(Operator) > 3 THEN KSL(Operator) = 3
END IF
IF cmd$ = "d" AND SelectedItem = 7 THEN
KSL(Operator) = KSL(Operator) - 1
IF KSL(Operator) < 0 THEN KSL(Operator) = 0
END IF

IF cmd$ = "u" AND SelectedItem = 8 THEN
FeedBack = FeedBack + 1
IF FeedBack > 7 THEN FeedBack = 7
END IF
IF cmd$ = "d" AND SelectedItem = 8 THEN
FeedBack = FeedBack - 1
IF FeedBack < 0 THEN FeedBack = 0
END IF

IF LCASE$(k) = "s" THEN
IF SustainKeep(Operator) = 1 THEN
SustainKeep(Operator) = 0
ELSE
SustainKeep(Operator) = 1
END IF
END IF

IF LCASE$(k) = "v" THEN
IF Vibrato(Operator) = 1 THEN
Vibrato(Operator) = 0
ELSE
Vibrato(Operator) = 1
END IF
END IF

IF LCASE$(k) = "a" THEN
IF AM(Operator) = 1 THEN
AM(Operator) = 0
ELSE
AM(Operator) = 1
END IF
END IF

IF LCASE$(k) = "d" THEN
IF DecayAlg = 1 THEN
DecayAlg = 0
ELSE
DecayAlg = 1
END IF
END IF

IF LCASE$(k) = "h" THEN
IF LHPiano = 1 THEN
LHPiano = 0
ELSE
LHPiano = 1
END IF
END IF

LOCATE 1, 1
PRINT "Operator "; (Operator + 1);

LOCATE 1, 55
COLOR 6
PRINT "(C) 2018, Vulpes SARL"
COLOR 7
LOCATE 2, 55
COLOR 1
PRINT "https://vulpes.lu"
COLOR 7

LOCATE 10, 60
COLOR 14
PRINT "L";
COLOR 7
PRINT "oad Instr";

LOCATE 5, 60
COLOR 14
PRINT "S";
COLOR 7
PRINT "ustain ";

IF SustainKeep(Operator) = 1 THEN PRINT "[X]";
IF SustainKeep(Operator) = 0 THEN PRINT "[ ]";

LOCATE 6, 60
COLOR 14
PRINT "V";
COLOR 7
PRINT "ibrato ";

IF Vibrato(Operator) = 1 THEN PRINT "[X]";
IF Vibrato(Operator) = 0 THEN PRINT "[ ]";

LOCATE 7, 60
COLOR 14
PRINT "A";
COLOR 7
PRINT "M      ";

IF AM(Operator) = 1 THEN PRINT "[X]";
IF AM(Operator) = 0 THEN PRINT "[ ]";

LOCATE 8, 60
COLOR 14
PRINT "D";
COLOR 15
PRINT "ecayAlg";

IF DecayAlg = 1 THEN PRINT "[X]";
IF DecayAlg = 0 THEN PRINT "[ ]";

LOCATE 9, 60
PRINT "L";
COLOR 14
PRINT "H";
COLOR 15
PRINT " Piano";

IF LHPiano = 1 THEN PRINT "[X]";
IF LHPiano = 0 THEN PRINT "[ ]";

LOCATE 15, 60
COLOR 14
PRINT "F1   ";
COLOR 7
PRINT "Help";

LOCATE 16, 60
COLOR 14
PRINT "ESC  ";
COLOR 7
PRINT "Exit";

LOCATE 17, 60
COLOR 14
PRINT CHR$(27) + " " + CHR$(26) + "  ";
COLOR 7
PRINT "Select Slider";

LOCATE 18, 60
COLOR 14
PRINT CHR$(24) + " " + CHR$(25) + "  ";
COLOR 7
PRINT "Adjust Slider";

LOCATE 19, 60
COLOR 14
PRINT "PgUp ";
COLOR 7
PRINT "Change Operator";

LOCATE 20, 60
COLOR 14
PRINT "PgDn ";
COLOR 7
PRINT "Change Operator";

IF OPL2LPT = 1 THEN
LOCATE 24, 60
PRINT "OPL2LPT Mode";
END IF

DrawScroller "Wav", 8, IIF(SelectedItem = 0, 14, 7), Wave(Operator), 3
DrawScroller "Att", 12, IIF(SelectedItem = 1, 14, 7), Attack(Operator), 15
DrawScroller "Dec", 16, IIF(SelectedItem = 2, 14, 7), Decay(Operator), 15
DrawScroller "Sus", 20, IIF(SelectedItem = 3, 14, 7), Sustain(Operator), 15
DrawScroller "Rel", 24, IIF(SelectedItem = 4, 14, 7), Release(Operator), 15
DrawScroller "Mul", 28, IIF(SelectedItem = 5, 14, 7), Multi(Operator), 15
DrawScroller "Tot", 32, IIF(SelectedItem = 6, 14, 7), Total(Operator), 63
DrawScroller "KSL", 36, IIF(SelectedItem = 7, 14, 7), KSL(Operator), 3
DrawScroller "FeB", 42, IIF(SelectedItem = 8, 14, 15), FeedBack, 7

IF cmd$ = "d" OR cmd$ = "u" OR LCASE$(k) = "s" OR LCASE$(k) = "v" OR LCASE$(k) = "a" OR LCASE$(k) = "d" THEN
WriteADSR Operator, Attack(Operator), Decay(Operator), Sustain(Operator), Release(Operator), Wave(Operator), Multi(Operator), Total(Operator), SustainKeep(Operator), Vibrato(Operator), AM(Operator), KSL(Operator), DecayAlg, FeedBack
END IF

IF LCASE$(k) = "l" THEN LoadInstrument
IF cmd$ = "help" THEN ShowHelp

END SUB

SUB DrawScroller (Text AS STRING, X AS INTEGER, colour AS INTEGER, Value AS INTEGER, max AS INTEGER)
COLOR colour
LOCATE 2, X
PRINT Text
LOCATE 3, X
PRINT CHR$(24)

FOR i = 22 TO 4 STEP -1
LOCATE i, X
PRINT CHR$(176)
NEXT i

fill% = (19 / max) * Value

count = 0
FOR i = 22 TO 4 STEP -1
count = count + 1
IF count > fill% THEN EXIT FOR
LOCATE i, X
PRINT CHR$(178)
NEXT i

LOCATE 23, X
PRINT CHR$(25)

LOCATE 24, X - 1
PRINT Value;
COLOR 7
END SUB

FUNCTION IIF% (condition, true, false)
IF condition THEN
IIF = true
ELSE
IIF = false
END IF
END FUNCTION

SUB LoadInstrument

DIM Instruments(0 TO 127) AS STRING
DIM InstData AS STRING * 24
DIM InstName AS STRING * 16

OPEN "INST.DAT" FOR BINARY AS #1
FOR i = 0 TO 127
GET #1, , InstData
GET #1, , InstName
Instruments(i) = InstName
NEXT i
CLOSE #1

DO
DO

COLOR 15, 1
FOR i = 0 TO 15
LOCATE i + 5, 20
IF ListS = i THEN COLOR 1, 15 ELSE COLOR 15, 1
PRINT Instruments(i + ListI);
NEXT i

key$ = INKEY$
LOOP WHILE key$ = ""
cmd$ = ""

IF LEN(key$) = 2 THEN
IF ASC(MID$(key$, 1, 1)) = 0 THEN
SELECT CASE ASC(MID$(key$, 2, 1))
CASE &H48 'up
cmd$ = "u"
CASE &H4B 'left
cmd$ = "l"
CASE &H4D 'right
cmd$ = "r"
CASE &H50 'down
cmd$ = "d"
CASE &H49 'page up
cmd$ = "-"
CASE &H51 'page down
cmd$ = "+"
END SELECT
END IF
END IF

IF cmd$ = "u" THEN
ListS = ListS - 1
IF ListS < 0 THEN
ListS = 0
ListI = ListI - 1
IF ListI < 0 THEN
ListI = 0
END IF
END IF
END IF

IF cmd$ = "d" THEN
ListS = ListS + 1
IF ListS > 15 THEN
ListI = ListI + 1
ListS = 15
IF ListI > 127 - 15 THEN
ListI = 127 - 15
END IF
END IF
END IF

IF cmd$ = "-" THEN
ListI = ListI - 10
IF ListI < 0 THEN
ListI = 0
END IF
END IF

IF cmd$ = "+" THEN
ListI = ListI + 10
IF ListI > 127 - 15 THEN
ListI = 127 - 15
END IF
END IF

IF key$ = CHR$(&HD) THEN

OPEN "INST.DAT" FOR BINARY AS #1
FOR i = 0 TO ListI + ListS
GET #1, , InstData
GET #1, , InstName
NEXT i
CLOSE #1

AM(0) = ASC(MID$(InstData, 1, 1))
Vibrato(0) = ASC(MID$(InstData, 2, 1))
SustainKeep(0) = ASC(MID$(InstData, 3, 1))
Multi(0) = ASC(MID$(InstData, 4, 1))

AM(1) = ASC(MID$(InstData, 5, 1))
Vibrato(1) = ASC(MID$(InstData, 6, 1))
SustainKeep(1) = ASC(MID$(InstData, 7, 1))
Multi(1) = ASC(MID$(InstData, 8, 1))

KSL(0) = ASC(MID$(InstData, 9, 1))
Total(0) = ASC(MID$(InstData, 10, 1))

KSL(1) = ASC(MID$(InstData, 11, 1))
Total(1) = ASC(MID$(InstData, 12, 1))

Attack(0) = ASC(MID$(InstData, 13, 1))
Decay(0) = ASC(MID$(InstData, 14, 1))

Attack(1) = ASC(MID$(InstData, 15, 1))
Decay(1) = ASC(MID$(InstData, 16, 1))

Sustain(0) = ASC(MID$(InstData, 17, 1))
Release(0) = ASC(MID$(InstData, 18, 1))

Sustain(1) = ASC(MID$(InstData, 19, 1))
Release(1) = ASC(MID$(InstData, 20, 1))

Wave(0) = ASC(MID$(InstData, 21, 1))
Wave(1) = ASC(MID$(InstData, 22, 1))
                                    
FeedBack = ASC(MID$(InstData, 23, 1))
DecayAlg = ASC(MID$(InstData, 24, 1))

WriteADSR 0, Attack(0), Decay(0), Sustain(0), Release(0), Wave(0), Multi(0), Total(0), SustainKeep(0), Vibrato(0), AM(0), KSL(0), DecayAlg, FeedBack
WriteADSR 1, Attack(1), Decay(1), Sustain(1), Release(1), Wave(1), Multi(1), Total(1), SustainKeep(1), Vibrato(1), AM(1), KSL(1), DecayAlg, FeedBack

EXIT DO

END IF

LOOP UNTIL key$ = CHR$(27)

COLOR 7, 0
CLS
RedrawScreen = 1
END SUB

SUB PlayNote (Channel AS INTEGER, Note AS INTEGER)
IF Note = -1 THEN 'Shutup
WriteAdlib &HB0 + Channel, 0
EXIT SUB
END IF

tone# = Note
IF LHPiano = 1 THEN tone# = 128 - tone#
chbend# = 0

Freq# = 172.00093# * EXP(.057762265# * (tone# + chbend#))

X = &H2000
WHILE Freq# >= 1023.5
Freq# = Freq# / 2
X = X + &H400
WEND
X = X + CINT(Freq#)
WriteAdlib &HA0 + Channel, X AND &HFF
X = X \ &H100
WriteAdlib &HB0 + Channel, X AND &HFF

END SUB

SUB ShowHelp
key$ = ""
page = 1
DO
COLOR 15, 1
CLS

IF page = 1 THEN

PRINT "Test"
PRINT "Page 1 of 2"
PRINT ""
PRINT CHR$(24) + " " + CHR$(25) + " Change pages";


END IF

IF page = 2 THEN

PRINT "Test 2"
PRINT "Page 2 of 2"
PRINT ""
PRINT CHR$(24) + " " + CHR$(25) + " Change pages";


END IF

        DO
        key$ = INKEY$
        IF key$ = CHR$(27) THEN EXIT DO
        IF key$ = CHR$(0) + CHR$(&H48) THEN EXIT DO
        IF key$ = CHR$(0) + CHR$(&H50) THEN EXIT DO
        LOOP

IF key$ = CHR$(27) THEN EXIT DO
IF key$ = CHR$(0) + CHR$(&H48) THEN
page = page - 1
IF page < 1 THEN page = 1
END IF

IF key$ = CHR$(0) + CHR$(&H50) THEN
page = page + 1
IF page > 2 THEN page = 2
END IF

LOOP
COLOR 7, 0
CLS
RedrawScreen = 1
END SUB

SUB WriteAdlib (Reg AS INTEGER, Value AS INTEGER)

IF OPL2LPT = 0 THEN
OUT &H388, Reg
OUT &H388 + 1, Value
END IF

IF OPL2LPT = 1 THEN
OUT &H378 + 0, Reg
OUT &H378 + 2, &HD
a = INP(&H378 + 2)
a = INP(&H378 + 2)
OUT &H378 + 2, &H9
a = INP(&H378 + 2)
a = INP(&H378 + 2)
OUT &H378 + 2, &HD
FOR i = 0 TO 7
        a = INP(&H378 + 2)
NEXT i

OUT &H378 + 0, Value
OUT &H378 + 2, &HC
a = INP(&H378 + 2)
a = INP(&H378 + 2)
OUT &H378 + 2, &H8
a = INP(&H378 + 2)
a = INP(&H378 + 2)
OUT &H378 + 2, &HC
FOR i = 0 TO 36
        a = INP(&H378 + 2)
NEXT i
END IF

END SUB

SUB WriteADSR (O AS INTEGER, a AS INTEGER, D AS INTEGER, S AS INTEGER, r AS INTEGER, W AS INTEGER, M AS INTEGER, T AS INTEGER, SK AS INTEGER, Vib AS INTEGER, AM AS INTEGER, KSL AS INTEGER, Alg AS INTEGER, FB AS INTEGER)
IF O = 0 THEN
WriteADSR2 &H0, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H1, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H2, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H8, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H9, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &HA, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H10, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H11, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H12, a, D, S, r, W, M, T, SK, Vib, AM, KSL
END IF
IF O = 1 THEN
WriteADSR2 &H3, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H4, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H5, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &HB, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &HC, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &HD, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H13, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H14, a, D, S, r, W, M, T, SK, Vib, AM, KSL
WriteADSR2 &H15, a, D, S, r, W, M, T, SK, Vib, AM, KSL
END IF

WriteDecayAlgFeedback Alg, FB

END SUB

SUB WriteADSR2 (i AS INTEGER, a AS INTEGER, D AS INTEGER, S AS INTEGER, r AS INTEGER, W AS INTEGER, M AS INTEGER, T AS INTEGER, SK AS INTEGER, Vib AS INTEGER, AM AS INTEGER, KSL AS INTEGER)
WriteAdlib &H20 + i, (64 * Vib) + (128 * AM) + (32 * SK) + M
WriteAdlib &H40 + i, 64 * KSL + T
WriteAdlib &H60 + i, 16 * a + D
WriteAdlib &H80 + i, 16 * S + r
WriteAdlib &HE0 + i, W
END SUB

SUB WriteDecayAlgFeedback (Alg AS INTEGER, FB AS INTEGER)
FOR i = 0 TO 8
WriteAdlib &HC0 + i, FB * 2 + Alg
NEXT i
END SUB

