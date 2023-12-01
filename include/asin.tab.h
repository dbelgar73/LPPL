/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_ASIN_TAB_H_INCLUDED
# define YY_YY_ASIN_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    APAR_ = 258,                   /* APAR_  */
    CPAR_ = 259,                   /* CPAR_  */
    MAS_ = 260,                    /* MAS_  */
    MENOS_ = 261,                  /* MENOS_  */
    POR_ = 262,                    /* POR_  */
    DIV_ = 263,                    /* DIV_  */
    ALLAVE_ = 264,                 /* ALLAVE_  */
    CLLAVE_ = 265,                 /* CLLAVE_  */
    ACOR_ = 266,                   /* ACOR_  */
    CCOR_ = 267,                   /* CCOR_  */
    PCOM_ = 268,                   /* PCOM_  */
    COM_ = 269,                    /* COM_  */
    IG_ = 270,                     /* IG_  */
    MAY_ = 271,                    /* MAY_  */
    MEN_ = 272,                    /* MEN_  */
    MAYIG_ = 273,                  /* MAYIG_  */
    MENIG_ = 274,                  /* MENIG_  */
    INC_ = 275,                    /* INC_  */
    DEC_ = 276,                    /* DEC_  */
    IGUALDAD_ = 277,               /* IGUALDAD_  */
    DIF_ = 278,                    /* DIF_  */
    OR_ = 279,                     /* OR_  */
    AND_ = 280,                    /* AND_  */
    NOT_ = 281,                    /* NOT_  */
    INT_ = 282,                    /* INT_  */
    BOOL_ = 283,                   /* BOOL_  */
    RETURN_ = 284,                 /* RETURN_  */
    IF_ = 285,                     /* IF_  */
    ELSE_ = 286,                   /* ELSE_  */
    PRINT_ = 287,                  /* PRINT_  */
    READ_ = 288,                   /* READ_  */
    FOR_ = 289,                    /* FOR_  */
    TRUE_ = 290,                   /* TRUE_  */
    FALSE_ = 291,                  /* FALSE_  */
    CTE_ = 292,                    /* CTE_  */
    ID_ = 293                      /* ID_  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_ASIN_TAB_H_INCLUDED  */
