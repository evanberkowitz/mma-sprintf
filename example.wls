#!/usr/bin/env wolframscript -print All 
(* ::Package:: *)

Needs["sprintf`"]


columns=StringRepeat["1234567890",8];


sprintf`sprintf["%s\n","After installing mathematica/sprintf.wl somewhere Mathematica looks, you can"] 


sprintf`sprintf["%.49s\n%s %S \n%40.36s\n%s %%%%\n",columns,
    "print strings with %s",
    "and capitalize them with %S",
    "strings can be aligned and truncated too.",
    "You can get a literal % by using"
    ]


sprintf`sprintf["%s %c\n%s %Ys",
    "You can also get single characters","!",
    "As a Mathematica-specific extension you can stringify",symbol
    ]


sprintf`sprintf["%s\n%.30s\n%10d%10i%10i  %s\n%+10d%+10i%+10i  %s\n",
    "You can format integers with %d or %i.",
    columns, 
    1234, -567890, 0,"You can omit the + flag and get only minus signs",
    1234, -567890, 0,"You can  use the + flag to demand signs."
    ]


sprintf`sprintf["%s\n%12s%8u\n%12s%8o\n%12s%8x\n%12s%8X\n",
    "You can format unsigned integers using %u for decimal, %o for octal, %x for hex, %X for HEX.",
    "%u",1234,
    "%o",1234,
    "%x",1234,
    "%X",1234
    ]


sprintf`sprintf["%s\n%.20s\n%9s %+10i\n%9s %+-10i\n%9s %+010i\n%9s %010i\n",
    "You can left align with a - flag, pad with 0s, and demand signs",columns,
    "%+8i",1234,
    "%+-8i",1234,
    "%+08i",1234,
    "%08i",1234
    ]


sprintf`sprintf["%s\n%.40s\n%20s%20s\n%4s%16f%20.3f\n%4s%16f%20.3f\n",
    "You can print things with default or .specified fixed precision",columns,
    "%f", "%.3f",
    "  \[Pi]=",  \[Pi],  \[Pi],
    "10\[Pi]=",10\[Pi],10\[Pi]
    ]


sprintf`sprintf["%s\n%.40s\n%4s%16e%20.3e\n%4s%16E%20.3E\n",
    "You can print things with default or .specified precision in scientific notation using %e and %E",columns,
    ".1\[Pi]=",.1\[Pi],.1\[Pi],
    "10\[Pi]=",10\[Pi],10\[Pi]
    ]


sprintf`sprintf["%s\n%4s%16a%20.8A\n",
    "You can print floats in hexadecimal using %a or %A.",
    "\[Pi]=",\[Pi],\[Pi]
    ]


sprintf`sprintf["%s\n%40s%30s\n%20s%20r%30.5r  %s\n%20s%20r%30.5r  %s\n%20s%20r%30.11r  %s\n%20s%20r%30.8r  %s\n%20s%20r%30.2r  %s",
    "As a Mathematica extension, here's an uncertainty format %r that consumes two numbers, also understands .precision.",
    "%r", "%.[varying precisions]r",
    "gA=", 1.271, 0.013, 1.271, 0.013,"If you request more digits than known, you get zeros.",
    "proton mass/MeV=", 938.27208816, 0.00000029, 938.27208816, 0.00000029, "If the uncertainty smaller than the requested precision, the result may look exact.",
    "proton mass/ kg=", 1.67262192369*^-27, 0.00000000051*^-27,1.67262192369*^-27, 0.00000000051*^-27, "If more digits of uncertainty are available, you can force them to appear.",
    "proton charge/ C=",1.6021766208*^-19,98*^-28,1.6021766208*^-19,98*^-28,"If the uncertainty starts with a 9 an extra digit is printed.",
    "charge radius/fm=", 0.8414,0.0019,0.8414,0.0019, "If the uncertainty starts with a 1 an extra digit is printed."
    ]
