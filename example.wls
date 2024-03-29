#!/usr/bin/env wolframscript 
(* ::Package:: *)

Needs["sprintf`"]


columnsUp=StringRepeat["1234567890",8];
columnsDown=StringRepeat["0987654321",8];


Echo@sprintf["%s\n","After installing mathematica/sprintf.wl somewhere Mathematica looks, you can"] 


Echo@sprintf["\n%.49s\n%s %S \n%40.36s.\n%s %%%%.\n",columnsUp,
    "print strings with %s",
    "and capitalize them with %S",
    "strings can be aligned and truncated too",
    "You can get a literal % by using"
    ]


Echo@sprintf["%s%c\n%s %Ys\n",
    "You can also get single characters","!",
    "As a Mathematica-specific extension you can stringify",symbol
    ]


Echo@sprintf["%s\n%.30s\n%10s%10s%10s\n%10d%10i%10i  %s\n%+10d%+10i%+10i  %s\n",
    "You can format integers with %d or %i.",
    columnsDown,
    "%10d", "%10i", "%10i",
    1234, -567890, 0,"You can omit the + flag and get only minus signs",
    1234, -567890, 0,"You can  use the + flag to demand signs."
    ]


Echo@sprintf["%s\n%10s%10u\n%10s%10o\n%10s%10x\n%10s%10X\n",
    "You can format unsigned integers using %u for decimal, %o for octal, %x for hex, %X for HEX.",
    "%u",1234,
    "%o",1234,
    "%x",1234,
    "%X",1234
    ]


Echo@sprintf["%s\n%.20s\n%9s %+10i\n%9s %+-10i\n%9s %+010i\n%9s %010i\n",
    "You can left align with a - flag, pad with 0s, and demand signs",columnsDown,
    "%+10i",1234,
    "%+-10i",1234,
    "%+010i",1234,
    "%010i",1234
    ]


Echo@sprintf["%s\n%.60s\n%20s%20s%20s\n%10s%10f%20.3f%20.10f\n%10s%10f%20.3f%20.10f\n%14s%s",
    "You can print things with default or .specified fixed precision",columnsDown,
    "%f", "%.3f","%.10f",
    " -\[Pi]=", -\[Pi], -\[Pi],-\[Pi],
    "10\[Pi]=",10\[Pi],10\[Pi],10\[Pi],
    "","|----| 6 digits is the default precision."
    ]


With[{small=-\[Pi]/10,big=+10\[Pi]},
Echo@sprintf["%s\n%.60s\n%20s%20s%20s\n%8s%+12e%+20.3e%+20.10e%8s\n%8s%+12E%+20.3E%+20.10E%8s\n",
    "You can print things with default or .specified precision in scientific notation using %e and %E",columnsDown,
    "%+", "%+.3","%+.10",
    "-\[Pi]/10=",small,small,small, "e",
    "10\[Pi]=",big,big,big,"E"
    ]];


Echo@sprintf["%s\n%.40s\n%20s%20s\n%6s%14a%20.8A\n",
    "You can print floats in hexadecimal using %a or %A.",columnsDown,
    "%a", "%A",
    "\[Pi]=",\[Pi],\[Pi]
    ]


Echo@sprintf["%s\n%40s%30s\n%20s%20r%30.5r  %s\n%20s%20r%30.5r  %s\n%20s%20r%30.11r  %s\n%20s%20r%30.8r  %s\n%20s%20r%30.2r  %s\n",
    "As a Mathematica extension, here's an uncertainty format %r that consumes two numbers, also understands .precision.",
    "%r", "%.[varying precisions]r",
    "gA=", 1.271, 0.013, 1.271, 0.013,"If you request more digits than known, you get zeros.",
    "proton mass/MeV=", 938.27208816, 0.00000029, 938.27208816, 0.00000029, "If the uncertainty smaller than the requested precision, the result may look exact.",
    "proton mass/ kg=", 1.67262192369*^-27, 0.00000000051*^-27,1.67262192369*^-27, 0.00000000051*^-27, "If more digits of uncertainty are available, you can force them to appear.",
    "proton charge/ C=",1.6021766208*^-19,98*^-28,1.6021766208*^-19,98*^-28,"If the uncertainty starts with a 9 an extra digit is printed.",
    "charge radius/fm=", 0.8414,0.0019,0.8414,0.0019, "If the uncertainty starts with a 1 an extra digit is printed."
    ]
