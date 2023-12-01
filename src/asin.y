/*****************************************************************************/
/** Ejemplo  S E M - 2                    2023-2024 <jmbenedi@prhlt.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libgci.h"
%}

%union{
  int cent ; /*Valor de la constantee numerica entera para el terminal "cte" */
  char *ident ; /* Nombre del identificador */
  EXP expre; /*expresiones*/
  whileINST instwhile;
}

%token  PUNTO_ APAR_  CPAR_  MAS_  MENOS_  POR_  DIV_  ALLAVE_  CLLAVE_
%token  ACOR_ CCOR_  PCOM_  COM_  IG_  MAY_  MEN_  MAYIG_  MENIG_
%token  INC_  DEC_  IGUALDAD_  DIF_  OR_  AND_  NOT_  INT_  BOOL_
%token  RETURN_  IF_  ELSE_  PRINT_  READ_  WHILE_  TRUE_  FALSE_

%token<cent>  CTE_
%token<ident>  ID_

%type<cent> tipoSimp opLogic opIgual 
%type<cent> opRel opAd opMul opUna opIncre instSelec

%type<expr> expre expreLogic expreIgual expreRel expreAd
%type<expr> expreMul expreUna expreSufi const 

%type<instwhile> instIter 


%%
/*Completo*/
programa: 
    /*Inicializar variables globales del compilador*/
    /*Reservar espacio variables globales del programa*/
    /*Emitir el salto al comienzo de la funcion main*/
    { dvar = 0; si = 0;} 
    listDecla 
        {
            /*Comprobar si el programa tiene "main"*/
            /*Completar reserva espacio para variables globales programa*/
            /*Completar salto al comienzo de la funcion "main"*/
            if (verTDS) mostrarTDS(); 
            emite(FIN,crArgNul(),crArgNul(),crArgNul());
        }
    ;
/*Completo*/
listDecla: decla 
    | listDecla decla 
    ;
/*Completo*/
decla: declaVar 
    | declaFunc
    ;
/*NOOOOOOO Completo*/
declaVar: tipoSimp ID_ PCOM_ {
            if (! insTSimpleTDS($2, $1, dvar) )
                yyerror ("ID repetido");
            else dvar += TALLA_TIPO_SIMPLE;    
        }
    | tipoSimp ID_ ACOR_ CTE_ CCOR_ PCOM_ 
        {
            int nelem = $4;
            if ($4 <= 0) {
                yyerror("Talla inapropiada del array!");
                nelem = 0;
            }
            if ( ! insTVectorTDS($2, T_ARRAY, dvar, $1, nelem) ) {
                yyerror ("ID repetido!");
            }
            else dvar += nelem * TALLA_TIPO_SIMPLE;
        }
    | struct ALLAVE_ listCamp CLLAVE_ ID_ PCOM_
    ;
/*Completo*/
tipoSimp: INT_ {$$ = T_ENTERO;}
    | BOOL_ {$$ = T_LOGICO;}
    ;
/*Completo*/
listCamp: tipoSimp ID_ PCOM_ 
    | listCamp tipoSimp ID_ PCOM_
    ;

declaFunc: tipoSimp ID_ APAR_ paramForm CPAR_ ALLAVE_ declaVarLocal listInst RETURN_ expre PCOM_ CLLAVE_ 
    ;
/*Completo*/
paramForm:  
    | listParamForm
    ;
/*Completo*/
listParamForm: tipoSimp ID_ {printf("Parametro %dº es de tipo %s\n", contador++, $1);}
    | tipoSimp ID_ {$<orden>$=contador++}
    COM_ listParamForm
        {
            printf("Parametro %dº es de tipo %s\n", $<orden>3, $1);
        }
    ;
/*NOOOOO COMPLETO DECLAVARTEMP*/
declaVarLocal:  
    | declaVarLocal declaVar 
    ;
/*Completo*/
listInst: 
    | listInst inst 
    ;
/*Completo*/
inst: ALLAVE_ listInst CLLAVE_ 
    | instExpre 
    | instEntSal 
    | instSelec 
    | instIter 
    ;
/*Completo*/
instExpre: expre PCOM_ 
    | PCOM_ 
    ;
/*Completo*/
instEntSal: READ_ APAR_ ID_ CPAR_ PCOM_
        {
            SIMB sim = obtenerTDS($3);
            if (sim.tipo != T_ENTERO) {
                yyerror("Identificador no válido para la lectura. Debe ser de tipo entero");
            }
            emite(EREAD, crArgNul(), crArgNul(), crArgPos(sim.desp));
        }
    | PRINT_ APAR_ expre CPAR_ PCOM_ 
        {
            if($3.tipo != T_ERROR){
                if ($3.tipo != T_ENTERO) {
                    yyerror("La expresion a imprimir debe ser de tipo entero");
                }
            }
            emite(EWRITE, crArgNul(), crArgNul(), crArgPos($3.pos));
        }
    ;
/*Completo*/
instSelec: IF_ APAR_ expre CPAR_
        {
            if($3.tipo != T_ERROR){
                if ($3.tipo != T_LOGICO){
                    yyerror("Error, aprende a programar. Debe ser tipo logico.");
                }
            }
            $<cent>$ = creaLans(si);
            emite(EIGUAL, crArgPos($3.pos), crArgEnt(0), crArgEtq(-1));
        } 
        inst{
                                    $<cent>$ = creaLans(si);
                                    emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
                                    completaLans($<cent>5, crArgEnt(si));                                    
            }
        ELSE_ inst{
            completaLans($<cent>7, crArgEnt(si));
        }  
    ;
/*No0000 completo*/
instIter: WHILE_ APAR_ expre CPAR_ inst
    ;
/*No0000 completo*/
expre: expreLogic {$$.tipo = $1.tipo; $$.pos = $1.pos;}
    | ID_ IG_ expre 
    | ID_ ACOR_ expre CCOR_ IG_ expre 
    | ID_ PUNTO_ ID_ IG_ expre 
    ;
/*Completo*/
expreLogic: expreIgual {$$.tipo = $1.tipo; $$.pos = $1.pos;}
    | expreLogic opLogic expreIgual
        {
            $$.tipo = T_ERROR;
            if($1.tipo != T_ERROR && $3.tipo != T_ERROR){
                if (!(($1.tipo == T_LOGICO && $3.tipo == T_LOGICO)||($1.tipo == T_ENTERO && $3.tipo == T_ENTERO))){
                    yyerror("Error de tipo en la expresion logica");
                } else {
                    $$.tipo = T_LOGICO;
                }
            }
            $$.pos = creaVarTemp();
            emite(EASIG, crArgEnt(1), crArgNul(), crArgPos($$.pos));
            emite($2, crArgPos($1.pos), crArgPos($3.pos), crArgEtq(si+2));
            emite(EASIG, crArgEnt(0), crArgNul(), crArgPos($$.pos));
        }
    ;
/*Completo*/
expreIgual: expreRel {$$.tipo = $1.tipo; $$.pos = $1.pos;}
    | expreIgual opIgual expreRel
        {
            $$.tipo = T_ERROR;
            if($1.tipo != T_ERROR && $3.tipo != T_ERROR){
                if (!(($1.tipo == T_LOGICO && $3.tipo == T_LOGICO)||($1.tipo == T_ENTERO && $3.tipo == T_ENTERO))){
                    yyerror("Error de tipo en la expresion de igualdad");
                } else {
                    $$.tipo = T_LOGICO;
                }
            }
            $$.pos = creaVarTemp();
            emite(EASIG, crArgEnt(1), crArgNul(), crArgPos($$.pos));
            emite($2, crArgPos($1.pos), crArgPos($3.pos), crArgEtq(si+2));
            emite(EASIG, crArgEnt(0), crArgNul(), crArgPos($$.pos));
        }
    ;
/*Completo*/
expreRel: expreAd {$$.tipo = $1.tipo; $$.pos = $1.pos;}
    | expreRel opRel expreAd
        {   
            $$.tipo = T_ERROR;
            if($1.tipo != T_ERROR && $3.tipo != T_ERROR){
                if (!(($1.tipo == T_LOGICO && $3.tipo == T_LOGICO)||($1.tipo == T_ENTERO && $3.tipo == T_ENTERO))){
                    yyerror("Error de tipos en la expresion relacional");
                } else {
                    $$.tipo = T_LOGICO;
                }
            }
            $$.pos = creaVarTemp();
            emite(EASIG, crArgEnt(1), crArgNul(), crArgPos($$.pos));
            emite($2, crArgPos($1.pos), crArgPos($3.pos), crArgEtq(si+2));
            emite(EASIG, crArgEnt(0), crArgNul(), crArgPos($$.pos));
        }
    ;
/*Completo*/
expreAd: expreMul {$$.tipo = $1.tipo; $$.pos = $1.pos;}
    | expreAd opAd expreMul 
        {
            $$.t = T_ERROR;
            if (($1.t == T_ENTERO) && ($3.t == T_ENTERO)) $$.t = T_ENTERO
            else yyerror("Error de tipos en la 'expresión aditiva'");

            $$.d = creaVarTemp();
            /*Expresion a partir de un operador aritmetico*/
            emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
        }
    ;    
/*Completo*/
expreMul: expreUna 
        {
            $$.tipo = $1.tipo;
            $$.pos = $1.pos;
        }
    | expreMul opMul expreUna
        {
            $$.tipo = T_ERROR;
            if($3.tipo != T_ERROR && $1.tipo != T_ERROR){
                if (!($1.tipo == T_ENTERO && $3.tipo == T_ENTERO)){
                    yyerror("Error de tipos en la expresion multiplicativa");
                } else {
                    $$.tipo = $1.tipo;
                }
            }
            $$.pos = creaVarTemp();
            emite($2, crArgPos($1.pos), crArgPos($3.pos), crArgPos($$.pos));
        }
    ;
/*Completo*/
expreUna: expreSufi {$$.tipo = $1.tipo; $$.pos = $1.pos;}
    | opUna expreUna 
        {
           $$.tipo = T_ERROR;
            if($2.tipo != T_ERROR){
                if (($1 == ESUM || $1 == EDIF) && $2.tipo == T_ENTERO) {
                    $$.tipo = $2.tipo;
                } else if ($1 == NOT && $2.tipo == T_LOGICO) {
                    $$.tipo = $2.tipo;
                } else {
                    yyerror("Error de tipos en la expresion unaria.");
                } 
            }
            $$.pos = creaVarTemp();
            if ($1 == NOT) {
                emite(EDIF, crArgEnt(1), crArgPos($2.pos), crArgPos($$.pos));    
            } else {
                emite($1, crArgEnt(0), crArgPos($2.pos), crArgPos($$.pos));
            }
        }    
    | opIncre ID_
        {
            $$.tipo = T_ERROR;
            SIMB simb = obtenerTDS($2);
            if (simb.tipo != T_ERROR) {
                if (simb.tipo == T_ENTERO) {
                    $$.tipo = simb.tipo;
                } else {
                    yyerror("No se puede incrementar algo que no sea de tipo entero");
                }
            }
            $$.pos = creaVarTemp();
            emite($1, crArgPos(simb.desp), crArgEnt(1), crArgPos(simb.desp));
            emite(EASIG, crArgPos(simb.desp), crArgNul(), crArgPos($$.pos));
        }
    ;
/*Completo*/
expreSufi: CTE_ 
        { 
            $$.tipo = $1.tipo; 
            $$.pos = creaVarTemp();
            emite(EASIG, crArgEnt($1.pos), crArgNul(), crArgPos($$.pos));
        }
    | APAR_ expre CPAR_
        {
            $$.tipo = $2.tipo;
            $$.pos = $2.pos;
        }
    | ID_ 
        {
            $$.tipo = T_ERROR;
            SIMB sim = obtenerTDS($1);
            if(sim.tipo != T_ERROR){
                $$.tipo = sim.tipo;
            }else{
                yyerror("Identificador no declarado");
            }
            $$.pos = creaVarTemp();
            emite(EASIG, crArgPos(sim.desp), crArgNul(), crArgPos($$.pos)); 
                                 
        }
    | ID_ opIncre
        {
            $$.tipo = T_ERROR;
            SIMB simb = obtenerTDS($1);
            if (simb.tipo != T_ERROR){
                if(simb.tipo == T_ENTERO){
                    $$.tipo = simb.tipo;
                } else {
                    yyerror("Error de tipos en la expresion sufija");
                }
            }
            //Propaga el valor de la variable y después incrementa el valor de ésta
            $$.pos = creaVarTemp();
            emite(EASIG, crArgPos(simb.desp), crArgNul(), crArgPos($$.pos)); 
            emite($2, crArgPos(simb.desp), crArgEnt(1), crArgPos(simb.desp)); 
        }
    | ID_ PUNTO_ ID_
    ;
/*Completo*/
const: CTE_ {$$.tipo = T_ENTERO; $$.pos = $1;}
    | TRUE_  {$$.tipo = T_LOGICO; $$.pos = 1;}
    | FALSE_ {$$.tipo = T_LOGICO; $$.pos = 0;}
    ;
/*Completo*/
paramAct: 
    | listParamAct
    ;
/*Completo*/
listParamAct: expre 
    | expre COM_ listParamAct
    ;
/*Completo*/
opLogic: AND_ {$$ = AND;}
    | OR_ {$$ = OR;} 
    ;
/*Completo*/
opIgual: IGUALDAD_ {$$ = EIGUAL;}
    | DIF_ {$$ = EDIST;}
    ;
/*Completo*/
opRel: MAY_ {$$ = EMAY}
    | MEN_ {$$ = EMEN;}
    | MAYIG_ {$$ = EMAYEQ;}
    | MENIG_ {$$ = EMENEQ;}
    ;
/*Completo*/
opAd: MAS_ {$$ = ESUM;}
    | MENOS_ {$$ = EDIF;}
    ;
/*Completo*/
opMul: POR_ {$$ = EMUL;}
    | DIV_ {$$ = EDIV;}
    ;
/*Completo*/
opUna: MAS_ {$$ = ESUM;}
    | MENOS_ {$$ = EDIF;}
    | NOT_ {$$ = NOT;}
    ;
/*Completo*/
opIncre: INC_ {$$ = ESUM;}
    | DEC_ {$$ = EDIF;}
    ;

%%
