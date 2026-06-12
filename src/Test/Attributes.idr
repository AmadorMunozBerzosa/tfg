||| Collection of proofs about attribute detection and validation

import DSL
import Implementation

canUseGlobalAttribute : errors (style [lang "es"]) === []
canUseGlobalAttribute = Refl

canUseSpecificAttribute : errors (style [blocking "render"]) === []
canUseSpecificAttribute = Refl 

cantUseIncorrectAttribute : errors (style [for ""]) === [InvalidAttribute For]
cantUseIncorrectAttribute = Refl

anything: matches Id Anything _ = True
anything = Refl

text: (
    matches Id (Text "abc") "abc" = True,
    matches Id (Text "abc") "AbC" = True,
    matches Id (Text "abc") "abd" = False
    )
text = (Refl, Refl, Refl)

trimmed: (
    matches Id (Trimmed "abc") "abc" = True,
    matches Id (Trimmed "abc") "AbC" = True,
    matches Id (Trimmed "abc") " abc" = True,
    matches Id (Trimmed "abc") "\tabc" = True,
    matches Id (Trimmed "abc") "abc " = True,
    matches Id (Trimmed "abc") "abc \r\n " = True,
    matches Id (Trimmed "abc") "ab c" = False
    )
trimmed = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

whitespaceBetween: (
    matches Id (WhitespaceBetween "a" "b") "ab" = True,
    matches Id (WhitespaceBetween "a" "b") "AB" = True,
    matches Id (WhitespaceBetween "a" "b") "a b" = True,
    matches Id (WhitespaceBetween "a" "b") "a\nb" = True,
    matches Id (WhitespaceBetween "a" "b") "a\n bc" = False
    )
whitespaceBetween = (Refl, Refl, Refl, Refl, Refl)

prefix': (
    matches Id (Prefix "ab") "abc" = True,
    matches Id (Prefix "aB") "AbC" = True,
    matches Id (Prefix "aB") "cab" = False
    )
prefix' = (Refl, Refl, Refl)

singleLine: (
    matches Id SingleLine "" = True,
    matches Id SingleLine "abc" = True,
    matches Id SingleLine "abc \t " = True,
    matches Id SingleLine "abc \r\n" = False
    )
singleLine = (Refl, Refl, Refl, Refl)

nowhitespace: (
    matches Id NoWhitespace "" = False,
    matches Id NoWhitespace "abc" = True,
    matches Id NoWhitespace "a bc" = False,
    matches Id NoWhitespace "a\nbc" = False,
    matches Id NoWhitespace "abc\t" = False
    )
nowhitespace = (Refl, Refl, Refl, Refl, Refl)

nonNegative: (
    matches Id NonNegative "abc" = False,
    matches Id NonNegative "1a" = False,
    matches Id NonNegative "1234567890" = True,
    matches Id NonNegative "0" = True,
    matches Id NonNegative "-1" = False,
    matches Id NonNegative "0." = False
    )
nonNegative = (Refl, Refl, Refl, Refl, Refl, Refl)

positive: (
    matches Id Positive "abc" = False,
    matches Id Positive "1a" = False,
    matches Id Positive "1234567890" = True,
    matches Id Positive "0" = False,
    matches Id Positive "-1" = False,
    matches Id Positive "0." = False
    )
positive = (Refl, Refl, Refl, Refl, Refl, Refl)

integer: (
    matches Id Integer' "abc" = False,
    matches Id Integer' "1a" = False,
    matches Id Integer' "0" = True,
    matches Id Integer' "-1" = True,
    matches Id Integer' "0." = False
    )
integer = (Refl, Refl, Refl, Refl, Refl)

float: (
    matches Id Float "abc" = False,
    matches Id Float "1a" = False,
    matches Id Float "0" = True,
    matches Id Float "-1" = True,
    matches Id Float "0.0" = True,
    matches Id Float "3.1E-2" = True,
    matches Id Float ".1" = True
    )
float = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

lessThan: (
    matches Id (LessThan 1) "0" = True,
    matches Id (LessThan 1) "1" = True,
    matches Id (LessThan 1) "abc" = False,
    matches Id (LessThan 1) "1a" = False,
    matches Id (LessThan 1) "2" = False,
    matches Id (LessThan 1) "-1" = True,
    matches Id (LessThan (-1)) "0.0" = False
    )
lessThan = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

greaterThan: (
    matches Id (GreaterThan 1) "2" = True,
    matches Id (GreaterThan 1) "1" = True,
    matches Id (GreaterThan 1) "abc" = False,
    matches Id (GreaterThan 1) "1a" = False,
    matches Id (GreaterThan 1) "-1" = False,
    matches Id (GreaterThan (-2)) "-1" = True,
    matches Id (GreaterThan (-1)) "-2.0" = False
    )
greaterThan = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

character: (
    matches Id Character "" = False,
    matches Id Character "ab" = False,
    matches Id Character "a" = True,
    matches Id Character " " = True
    )
character = (Refl, Refl, Refl, Refl)

list: (
    matches Id (List " " Character) "" = True,
    matches Id (List " " Character) "abc" = False,
    matches Id (List " " Character) "a b c" = True,
    matches Id (List " " Character) "a b c " = False,
    matches Id (List "," Character) "a,b,c" = True,
    matches Id (List " " Character) "a bc" = False,
    matches Id (List " " Character) "ab c" = False
    )
list = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

set: (
    matches Id (Set " " Character) "" = True,
    matches Id (Set " " Character) "abc" = False,
    matches Id (Set " " Character) "a b c" = True,
    matches Id (Set "," Character) "a,b,c" = True,
    matches Id (Set " " Character) "a bc" = False,
    matches Id (Set " " Character) "ab c" = False,
    matches Id (Set " " Character) "a b b" = False,
    matches Id (Set " " Character) "a b a" = False
    )
set = (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl)

boolean: (
    matches Download Boolean "" = True,
    matches Download Boolean "download" = True,
    matches Download Boolean "Download" = True,
    matches Id Boolean "id" = True,
    matches Download Boolean "a" = False
    )
boolean = (Refl,Refl,Refl,Refl,Refl)

size: (
    matches Id Size "" = False,
    matches Id Size "any" = True,
    matches Id Size "x" = False,
    matches Id Size "1x1" = True,
    matches Id Size "12x35" = True,
    matches Id Size "01x2" = False,
    matches Id Size "1x02" = False
    )
size = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

circleCoords: (
    matches Id CircleCoords "" = False,
    matches Id CircleCoords "abc" = False,
    matches Id CircleCoords "1 2" = False,
    matches Id CircleCoords "1 2 3 4" = False,
    matches Id CircleCoords "1 2 3" = True
    )
circleCoords = (Refl,Refl,Refl,Refl,Refl)

rectCoords: (
    matches Id RectCoords "" = False,
    matches Id RectCoords "abc" = False,
    matches Id RectCoords "1 2" = False,
    matches Id RectCoords "1 2 3" = False,
    matches Id RectCoords "1 2 3 4" = True, 
    matches Id RectCoords "3 2 1 4" = False,
    matches Id RectCoords "1 4 3 2" = False
    )
rectCoords = (Refl,Refl,Refl,Refl,Refl,Refl,Refl)

polygonCoords: (
    matches Id PolygonCoords "" = False,
    matches Id PolygonCoords "abc" = False,
    matches Id PolygonCoords "1 2" = True,
    matches Id PolygonCoords "1 2 3" = False,
    matches Id PolygonCoords "1 2 3 4" = True, 
    matches Id PolygonCoords "1 2 3 4 5" = False
    )
polygonCoords = (Refl,Refl,Refl,Refl,Refl,Refl)

navigableTargetName: (
    matches Id NavigableTargetName "_a" = False,
    matches Id NavigableTargetName "a_b" = False,
    matches Id NavigableTargetName "a\tb" = False,
    matches Id NavigableTargetName "a\nb" = False,
    matches Id NavigableTargetName "a<b" = False,
    matches Id NavigableTargetName "abc" = True
    )
navigableTargetName = (Refl,Refl,Refl,Refl,Refl,Refl)

month: (
    matches Id Month "" = False,
    matches Id Month "January" = False,
    matches Id Month "Jan" = False,
    matches Id Month "1" = False,
    matches Id Month "01" = True,
    matches Id Month "12" = True,
    matches Id Month "13" = False
    )
month = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

date: (
    matches Id Date "1-12-12" = False,
    matches Id Date "0001-12-12" = True,
    matches Id Date "1999-1-12" = False,
    matches Id Date "1999-12-12" = True,
    matches Id Date "1999-12-2" = False,
    matches Id Date "1999-12-32" = False,
    matches Id Date "1999-11-31" = False,
    matches Id Date "1999-02-29" = False,
    matches Id Date "2000-02-29" = True,
    matches Id Date "2004-02-29" = True,
    matches Id Date "2100-02-29" = False
    )
date = (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl)

time: (
    matches Id Time "" = False,
    matches Id Time "10" = False,
    matches Id Time "10:20" = True,
    matches Id Time "10:20:31" = True,
    matches Id Time "10:20:31:1" = True,
    matches Id Time "10:20:31:12" = True,
    matches Id Time "10:20:31:123" = True,
    matches Id Time "10:20:31:1234" = False,
    matches Id Time "10:2:31" = False,
    matches Id Time "1:22:31" = False
    )
time = (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl)


datetime: (
    matches Id Datetime "" = False,
    matches Id Datetime "1999-12-12" = False,
    matches Id Datetime "1999-12-12 10:20:20" = True,
    matches Id Datetime "1999-12-12T10:20:20" = True,
    matches Id Datetime "1999-12-32T10:20:20" = False,
    matches Id Datetime "1999-12-12T10:20:61" = False
    )
datetime = (Refl, Refl, Refl, Refl, Refl, Refl)

week: (
    matches Id Week "" = False,
    matches Id Week "1999-W0" = False,
    matches Id Week "1999-W01" = True,
    matches Id Week "1999-W52" = True,
    matches Id Week "1999-W53" = False,
    matches Id Week "2000-W01" = True
    )
week = (Refl, Refl, Refl, Refl, Refl, Refl)

(||): (
    matches Id (Text "any" || Character) "any" = True,
    matches Id (Text "any" || Character) "a" = True,
    matches Id (Text "any" || Character) "abc" = False
    )
(||) = (Refl,Refl,Refl)

autocomplete: (
    matches Id Autocomplete "" = False,
    matches Id Autocomplete "on" = True,
    matches Id Autocomplete "off" = True,
    matches Id Autocomplete "name" = True,
    matches Id Autocomplete "section-red given-name" = True,
    matches Id Autocomplete "shipping name" = True,
    matches Id Autocomplete "name webauthn" = True
    )
autocomplete = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)