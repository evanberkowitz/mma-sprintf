(* ::Package:: *)

BeginPackage["sprintf`"];

sprintf
fprintf
printf


Begin["Private`"];


FLAGS="[+0-]*"
LENGTHS="hh|ll|[hlLzjt]"
TYPES="[%sScCyYdiuoxXfFeEgGaArR]"

sprintf::usage="%[flags][width][.precision][length]type
	
	flags: "<>FLAGS<>"
		-        left align
		+        print a plus sign for positive numbers
		0        prepend zeros for numeric types, rather than spaces.
	
	width: either * or an unsigned integer.  Sets the minimum number of characters to output. 
        *        Consume a parameter, which should be an unsigned integer, and use that as the minimum width.
        integer  Use the integer directly.

    precision: either * or an integer.  
        *        Consume a parameter, which should be an unsigned integer, and use that as the precision.
        integer  Use the integer directly to set how many digits after the decimal point.

    length:  "<>LENGTHS<>"
        Currently does not do any thing at all.

    type:  "<>TYPES<>"  The convention is that capital letters result in capitalized results.
        %        Print a literal %
        STRINGS
        s        A literal string
        S        A literal string, capitalized.                                    EXTENSION!
        c        A single character.
        C        A single character, capitalized.                                  EXTENSION!
        y        Convert unevaluated symbols to strings.                           EXTENSION!
		INTEGERS
        d        signed int
        i        signed int
        u        unsigned int
        o        unsigned int in octal
        x        unsigned int in hexadecimal 0x0123456789abcdef
        X        unsigned int in hexadecimal 0X0123456789ABCDEF
        FLOATS
        f        fixed point, inf, nan
        F        fixed point, INF, NAN
        e        scientific notation with an e separator, int.0123456789e\[PlusMinus]power
        E        scientific notation with an E separator, int.0123456789E\[PlusMinus]power
        g
        G
        a        Hexadecimal float 0x[89abcdef].[0123456789abcdef]p\[PlusMinus](power of two)
        A        Hexadecimal float 0X[89ABCDEF].[0123456789ABCDEF]P\[PlusMinus](power of two)
        r        TWO fields, a central value and an error. central(error)e\[PlusMinus]power   EXTENSION!
        R        TWO fields, a central value and an error. central(error)E\[PlusMinus]power   EXTENSION!

This matches, to the best of my ability, bash's printf, plus useful-to-me extensions.
";


format=RegularExpression["([^%]*)(%("<>FLAGS<>")?(\\*|\\d*)?(\\.(\\*|\\d*))?(<>"<>LENGTHS<>"<>)?("<>TYPES<>"))([\\S\\s]*)"];

SIGNEDTYPES="dieEfFgGr";


BLANK="";


sprintf::unfilledfields="There were unused fields";
sprintf::ignoredfields="There were too many parameters.  The fields `1` were ignored.";
sprintf::unknowntype="The type `1` is unknown.";
sprintf::notstring="The argument `1` should have been a string.";
sprintf::notchar="The argument `1` should have been a char.";
sprintf::notinteger="The argument `1` should have been an integer.";
sprintf::notunsignedint="The argument `1` should have been an unsigned integer.";
sprintf::badfloatstring="The argument `1` is not a recognized float string representing infinity or not-a-number.";


(* Things that are already text: *)
precision["s"][PRECISION_][this_String]:=StringTake[this,Min[StringLength[this],PRECISION]];
precision["s"][PRECISION_][this_]:=(Message[sprintf::notstring,this];BLANK);
precision["c"][PRECISION_][this_String]:=precision["s"][1][this]
precision["c"][PRECISION_][this_]:=(Message[sprintf::notchar,this];BLANK);

(* May as well convert symbols to strings: *)
precision["y"][PRECISION_][this_Symbol]:=precision["s"][PRECISION][ToString[this]]


(* Integers *)
precision["d"][PRECISION_][this_Integer]:=ToString[this]
precision["d"][PRECISION_][this_]:=(Message[sprintf::notinteger,this];BLANK);
precision["i"]=precision["d"];

(* Unsigned Integers *)
precision["u"][PRECISION_][this_Integer/;this>=0]:=IntegerString[this]
precision["u"][PRECISION_][this_]:=(Message[sprintf::notunsignedint,this];BLANK);

(* Integers in hex*)
precision["x"][PRECISION_][this_Integer/;this>=0]:=IntegerString[this,16]
precision["x"][PRECISION_][this_]:=(Message[sprintf::notunsignedint,this];BLANK);

(* Integers in oct *)
precision["o"][PRECISION_][this_Integer/;this>=0]:=IntegerString[this,8]
precision["o"][PRECISION_][this_]:=(Message[sprintf::notunsignedint,this];BLANK);


(* Float *)
PRECISIONDEFAULT=6;

INFNAN[this]:=Switch[ToUpperCase[this],"INF","inf","NAN","nan",_,Message[sprintf::badfloatstring,this];BLANK];

precision["f"][PRECISION_][\[Infinity]]:="inf"
precision["f"][PRECISION_][this_String]:=INFNAN[this]
precision["f"][\[Infinity]][this_?NumericQ]:=precision["f"][PRECISIONDEFAULT][N[this,PRECISIONDEFAULT+2]];
precision["f"][PRECISION_][this_?NumericQ]:=ToString[IntegerPart[this]]<>"."<>StringPadRight[ToString[IntegerPart[FractionalPart[this] 10^PRECISION]],PRECISION,"0"]

precision["e"][PRECISION_][\[Infinity]]:="inf"
precision["e"][PRECISION_][this_String]:=INFNAN[this]
precision["e"][\[Infinity]][this_?NumericQ]:=precision["e"][PRECISIONDEFAULT][N[this,PRECISIONDEFAULT+2]];
precision["e"][PRECISION_][this_?NumericQ/;Abs[this]>=1]:=ToString[ScientificForm[N[this,PRECISION+2],PRECISION+1,NumberFormat->(Row[{
        #1,
        "e+",
        If[#3=="","0",#3]
       }]&)
    ]]
precision["e"][PRECISION_][this_?NumericQ/;Abs[this]<1]:=ToString[ScientificForm[N[this,PRECISION+2],PRECISION+1,NumberFormat->(Row[{
        #1,
        "e",
        If[#3=="","0",#3]
        }]&)
    ]]

(* "r" value(uncertainty)epower form *)
precision["r"][\[Infinity]][central_,error_]:=Module[{
        c=Floor@Log[10,central],
        e=Floor@Log[10,error]
    },
    If[Or[Ceiling[Abs[error]/10^e]==10,Floor[Abs[error]/10^e]==1],e-=1];
    precision["r"][c-e][central,error]
];        
precision["r"][PRECISION_][central_,error_]:=Module[{
        cdigits=Floor@Log[10,central],
        edigits=Floor@Log[10,Abs@error],
        pdigits=PRECISION
    },
    StringTemplate["`central`(`error`)e`size`"]@<|
        "central"->NumberForm[central/10^cdigits,{pdigits+1,pdigits}],
        "error"->NumberForm[Round[Abs@error/10^(cdigits-pdigits)]],
        "size"->If[cdigits>=0,"+"<>ToString[cdigits],ToString[cdigits]]
    |>
]


(* Hexadecimal *)
(* This was EXTRERMELY ANNOYING to get right! *)
precision["a"][PRECISION_][this_Integer]:=Module[{rd,offset,digits,first,rest,power},
    rd=RealDigits[this,2];
    offset=rd[[2]];
    digits=IntegerString[FromDigits[#,2]&/@Partition[rd[[1]],4,4,1,0],16];
    first=First[digits];
    rest=StringJoin[digits[[2;;]]];
    power=ToString@NumberForm[Floor@(Log[2,this/8]),NumberSigns->{"-","+"}];
    If[rest!="",rest="."<>rest];
    "0x"<>first<>rest<>"p"<>power
]

(* This still has some bugs: *)
precision["a"][PRECISION_][this_]:=Module[{default=PRECISIONDEFAULT,rd,offset,digits,first,rest,power},
    rd=RealDigits[this,2,Floor[4 Log[10,16] Switch[PRECISION,\[Infinity],default,_,PRECISION]]];
    offset=rd[[2]];
    digits=IntegerString[FromDigits[#,2]&/@Partition[rd[[1]],4,4,1,0],16];
    first=First[digits];
    rest=StringJoin[digits[[2;;Switch[PRECISION,\[Infinity],-1,_,PRECISION+1]]]];
    power=ToString@NumberForm[Floor@(Log[2,this/8]),NumberSigns->{"-","+"}];
    If[rest!="",rest="."<>rest];
    "0x"<>first<>rest<>"p"<>power
]

precision["A"][PRECISION_][this_]:=ToUpperCase[precision["a"][PRECISION][this]];


parse[s_]:=StringCases[s,format:>{
    "$1",
    "$2",
    "$9",
    "$3",
    Switch["$4","",-\[Infinity],"*","*",_,FromDigits["$4"]],
    Switch["$6","",\[Infinity],"*","*",_,FromDigits["$6"]],
    "$8"
    }][[1]];


flagParse[FLAG_]:={
    StringContainsQ[FLAG,"0"],
    StringContainsQ[FLAG,"-"],
    StringContainsQ[FLAG,"+"]
}


align[WIDTH_][LEADINGZEROES_,LEFTALIGN_,SIGNED_,SIGN_][replacement_]:=If[LEFTALIGN,
    Which[
        SIGNED, StringPadRight[SIGN<>replacement,Max[StringLength[replacement]+1,WIDTH]," "],
        True,   StringPadRight[SIGN<>replacement,Max[StringLength[SIGN<>replacement],WIDTH]," "]
    ],
    Which[
        LEADINGZEROES,           SIGN<>StringPadLeft[replacement,Max[StringLength[replacement],WIDTH-StringLength[SIGN]],"0"],
        True,                    StringPadLeft[SIGN<>replacement,Max[StringLength[SIGN<>replacement],WIDTH]," "]
    ]];


sprintf[s_String]:=StringReplace[s,"%%"->"%"]
sprintf[s_String, ignored__]:=(Message[sprintf::ignoredfields,{ignored}];s)/;StringFreeQ[s,"%"]


sprintf[s_String,this_,rest___]:=Module[
    {
        THIS,
        HEAD,
        TARGET,
        FLAG,LEFTALIGN,SIGNED,LEADINGZEROES,HASH,SIGN="",
        WIDTH,
        PRECISION,
        TYPE,CASE,
        TAIL
    },
    
    THIS=this;

    {HEAD,TARGET,TAIL,FLAG,WIDTH,PRECISION,TYPE}=parse[s];

    (*Print[{HEAD,TARGET,TAIL,FLAG,WIDTH,PRECISION,TYPE}];*)

    (* %% is a special type, escaping the % sign. *)
    If[TYPE=="%",Return[HEAD<>"%"<>sprintf[TAIL,THIS,rest]]];
    
    CASE=If[(ToUpperCase[TYPE]==TYPE),ToUpperCase,Identity];
    TYPE=ToLowerCase[TYPE];

    (* WIDTH has a special form * for dynamic precision, where it eats a parameter and uses it for formatting the NEXT thing. *)
    If[WIDTH=="*",
        If[ IntegerQ[THIS] && THIS>0,
            Return[HEAD<>sprintf[StringReplace[TARGET,"*"->IntegerString[THIS],1]<>TAIL,rest]],
            Message[sprintf::notunsignedint,THIS];
            Return[HEAD<>sprintf[TAIL,rest]];
        ]
    ];

    (* PRECISION has a special form * for dynamic precision, where it eats a parameter and uses it for formatting the NEXT thing. *)
    If[PRECISION=="*",
        If[ IntegerQ[THIS] && THIS>0,
            Return[HEAD<>sprintf[StringReplace[TARGET,"*"->IntegerString[THIS],1]<>TAIL,rest]],
            Message[sprintf::notunsignedint,THIS];
            Return[sprintf[HEAD<>TAIL,rest]];
        ]
    ];

    {LEADINGZEROES,LEFTALIGN,SIGNED}=flagParse[FLAG];

    If[ StringContainsQ[SIGNEDTYPES,TYPE] && NumericQ[THIS],
        SIGN=Switch[Sign[THIS], 
                -1, "-",
                _,  If[SIGNED,"+",""]];
        THIS=Abs[THIS];,
        SIGNED=False;
    ];

    If[TYPE=="r", Return[HEAD<>(align[WIDTH][LEADINGZEROES,LEFTALIGN,SIGNED,SIGN]@CASE@precision[TYPE][PRECISION][THIS,First@{rest}])<>sprintf[TAIL,Sequence@@(Rest@{rest})]]];

    HEAD<>(align[WIDTH][LEADINGZEROES,LEFTALIGN,SIGNED,SIGN]@CASE@precision[TYPE][PRECISION][THIS])<>sprintf[TAIL,rest]
    ]


fprintf[fileStream_,s_,fields___]:=WriteString[fileStream, sprintf[s,fields]];
printf[s_,fields___]:=fprintf[$Output,s,fields];


End[];
EndPackage[];
