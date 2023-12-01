/*****************************************************************************/
/**  Ejemplo de FLEX: S E M - 1           2023-2024 <jmbenedi@prhlt.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "asin.tab.h"

#define  retornar(x) {if (verbosidad) ECHO; return x;}
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

"+"	            { retornar (MAS_);      }
"."             { retornar (PUNTO_);    }
"-"             { retornar (MENOS_);    }
"*"             { retornar (POR_);      }
"/"             { retornar (DIV_);      }
"("             { retornar (APAR_);     }
")"             { retornar (CPAR_);     }
"{"	            { retornar (ALLAVE_);     }
"}"             { retornar (CLLAVE_);     }
"["             { retornar (ACOR_);       }
"]"             { retornar (CCOR_);       }
";"             { retornar (PCOM_);       }
","             { retornar (COM_);        }
"="	            { retornar (IG_);         }
">"             { retornar (MAY_);        }
"<"             { retornar (MEN_);        }
">="            { retornar (MAYIG_);      }
"<="            { retornar (MENIG_);      }
"++"            { retornar (INC_);        }
"--"	          { retornar (DEC_);        }
"=="            { retornar (IGUALDAD_);   }
"!="            { retornar (DIF_);        }
"||"            { retornar (OR_);         }
"&&"            { retornar (AND_);        }
"!"             { retornar (NOT_);        }

int             { retornar (INT_);        }
bool            { retornar (BOOL_);       }
return          { retornar (RETURN_);     }
if              { retornar (IF_);         }
else            { retornar (ELSE_);       }
print           { retornar (PRINT_);      }
read            { retornar (READ_);       }
while           { retornar (WHILE_);      }
true            { retornar (TRUE_);       }
false           { retornar (FALSE_);      }

{entero}       { retornar (CTE_);      }
{variable}     { retornar (ID_);       }

<<EOF>>        { if (verbosidad) { fprintf(stdout, "\n");}; return 0;}
.	             { yyerror("Caracter desconocido") ;}
%%