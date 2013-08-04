//@author <a href="mailto:wstarcev@gmail.com">Vasilii Startsev</a>
grammar AnimoGramm;

@header {
/*
 * Copyright (C) 2011-2013 The Animo Project
 * http://animotron.org
 *
 * This file is part of Animotron.
 *
 * Animotron is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * Animotron is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of
 * the GNU Affero General Public License along with Animotron.
 * If not, see <http://www.gnu.org/licenses/>.
 */

package org.animotron.lang.antlr;
}

prog : ( DEF def '.' )+ ;

def : ( action | many | point | action many | many point )? name ( usage | value | ',' value )* ;

value : ( action | many | point | action many | many point )? ( number_val | name | str | usage ) ;

usage : many? '(' ( link | link2 | context | value+ | many point ) ')' ;

action : GET | AN ;
many : ALL | ANY ;
point : THE | THIS ;

context : '^' name ;

link : LINK value+ ;
link2 : LINK LINK value+ ;

name : WORD ;

str : STR ;

number_val : NUMER ;

LINK : '-->' ;
ANY : 'any' ;
ALL : 'all' ;
THE : 'the' ;
THIS : 'this' ;
AN : 'an' ;
DEF : 'def' ;
GET : 'get' ;

WORD : [a-zA-Z]+ ;

NAME_LETTER : [a-zA-Z] ;
NUMER : [0-9] ;

NUMBER : '-'? ( '0' .. '9' )+ ( '.' ( '0' .. '9' )+ )? ( 'eE' '-'? ( '0' .. '9' )+ )? ;
LETTER : [a-zA-Z\u0080-\u00FF_@!#$%^&*\.] ;
STR : '"' ~'"'* '"' ;

STRING
	:	'"' ( ~( '"' | '\n' | '\r' ) )* '"'
	|	'\'' ( ~( '\'' | '\n' | '\r' ) )* '\''
	;

NUM
	:	'-' ( ( '0'..'9' )* '.' )? ( '0'..'9' )+
	|	( ( '0'..'9' )* '.' )? ( '0'..'9' )+
	;

WS : [ \n\r\t\,] -> channel(HIDDEN) ;

COMMENT : ';' ~[\r\n]* -> channel(HIDDEN) ;
