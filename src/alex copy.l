/*****************************************************************************/
/**  Ejemplo de FLEX: S E M - 1           2023-2024 <jmbenedi@prhlt.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"

%}
/*-----------------------------------------------------------------------------
  Estas opciones evitan "warnings" innecesarios y permiten obtener el
  numero de linea actual en la variable global "yylineno".
-----------------------------------------------------------------------------*/
%option noinput       
%option nounput       
%option yylineno

delimitador    [ \t\n]+ 
digito         [0-9]
entero         {digito}+
real           {digito}+(\.{digito}*)?|{digito}*\.{digito}+ 
variable       [_a-zA-Z][_a-zA-Z0-9]*
comentario     \/\/.*\n

%%
{delimitador}  { if (verbosidad) ECHO; }
{comentario}   { if (verbosidad) ECHO; fprintf(stdout, "%3d.- ", yylineno); } 

"+"	            { if (verbosidad) ECHO; return MAS_;      }
"-"             { if (verbosidad) ECHO; return MENOS_;    }
"*"             { if (verbosidad) ECHO; return POR_;      }
"/"             { if (verbosidad) ECHO; return DIV_;      }
"("             { if (verbosidad) ECHO; return x;APAR_;     }
")"             { if (verbosidad) ECHO; return CPAR_;     }
"{"	            { if (verbosidad) ECHO; return ALLAVE_;     }
"}"             { if (verbosidad) ECHO; return CLLAVE_;     }
"["             { if (verbosidad) ECHO; return ACOR_;       }
"]"             { if (verbosidad) ECHO; return CCOR_;       }
";"             { if (verbosidad) ECHO; return PCOM_;       }
","             { if (verbosidad) ECHO; return COM_;        }
"="	            { if (verbosidad) ECHO; return IG_;         }
">"             { if (verbosidad) ECHO; return MAY_;        }
"<"             { if (verbosidad) ECHO; return MEN_;        }
">="            { if (verbosidad) ECHO; return MAYIG_;      }
"<="            { if (verbosidad) ECHO; return MENIG_;      }
"++"            { if (verbosidad) ECHO; return INC_;        }
"--"	          { if (verbosidad) ECHO; return DEC_;        }
"=="            { if (verbosidad) ECHO; return IGUALDAD_;   }
"!="            { if (verbosidad) ECHO; return DIF_;        }
"||"            { if (verbosidad) ECHO; return OR_;         }
"&&"            { if (verbosidad) ECHO; return AND_;        }
"!"             { if (verbosidad) ECHO; return NOT_;        }

int             { if (verbosidad) ECHO; return INT_;        }
bool            { if (verbosidad) ECHO; return BOOL_;       }
return          { if (verbosidad) ECHO; return RETURN_;     }
if              { if (verbosidad) ECHO; return IF_;         }
else            { if (verbosidad) ECHO; return ELSE_;       }
print           { if (verbosidad) ECHO; return PRINT_;      }
read            { if (verbosidad) ECHO; return READ_;       }
for             { if (verbosidad) ECHO; return FOR_;        }
true            { if (verbosidad) ECHO; return TRUE_;       }
false           { if (verbosidad) ECHO; return FALSE_;      }

{entero}       { if (verbosidad) ECHO; return CTE_;      }
{variable}     { if (verbosidad) ECHO; return ID_;       }

<<EOF>>        { if (verbosidad) { fprintf(stdout, "\n");}; return 0;}
.	             {  if (verbosidad){
                  fprintf(stdout, "<"); ECHO; fprintf(stdout, "> "); }
                  yyerror("Caracter desconocido");
               }
%%
int verbosidad = TRUE;