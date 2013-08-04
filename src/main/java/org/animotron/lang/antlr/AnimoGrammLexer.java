// Generated from AnimoGramm.g by ANTLR 4.1

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

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNSimulator;
import org.antlr.v4.runtime.atn.LexerATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AnimoGrammLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__4=1, T__3=2, T__2=3, T__1=4, T__0=5, LINK=6, ANY=7, ALL=8, THE=9, THIS=10, 
		AN=11, DEF=12, GET=13, WORD=14, NAME_LETTER=15, NUMER=16, NUMBER=17, LETTER=18, 
		STR=19, STRING=20, NUM=21, WS=22, COMMENT=23;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'^'", "'.'", "')'", "','", "'('", "'-->'", "'any'", "'all'", "'the'", 
		"'this'", "'an'", "'def'", "'get'", "WORD", "NAME_LETTER", "NUMER", "NUMBER", 
		"LETTER", "STR", "STRING", "NUM", "WS", "COMMENT"
	};
	public static final String[] ruleNames = {
		"T__4", "T__3", "T__2", "T__1", "T__0", "LINK", "ANY", "ALL", "THE", "THIS", 
		"AN", "DEF", "GET", "WORD", "NAME_LETTER", "NUMER", "NUMBER", "LETTER", 
		"STR", "STRING", "NUM", "WS", "COMMENT"
	};


	public AnimoGrammLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "AnimoGramm.g"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 21: WS_action((RuleContext)_localctx, actionIndex); break;

		case 22: COMMENT_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WS_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: _channel = HIDDEN;  break;
		}
	}
	private void COMMENT_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 1: _channel = HIDDEN;  break;
		}
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\2\31\u00ca\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\3\2"+
		"\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3"+
		"\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3"+
		"\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\17\6\17]\n\17\r\17\16\17^\3\20\3"+
		"\20\3\21\3\21\3\22\5\22f\n\22\3\22\6\22i\n\22\r\22\16\22j\3\22\3\22\6"+
		"\22o\n\22\r\22\16\22p\5\22s\n\22\3\22\3\22\3\22\3\22\5\22y\n\22\3\22\6"+
		"\22|\n\22\r\22\16\22}\5\22\u0080\n\22\3\23\3\23\3\24\3\24\7\24\u0086\n"+
		"\24\f\24\16\24\u0089\13\24\3\24\3\24\3\25\3\25\7\25\u008f\n\25\f\25\16"+
		"\25\u0092\13\25\3\25\3\25\3\25\7\25\u0097\n\25\f\25\16\25\u009a\13\25"+
		"\3\25\5\25\u009d\n\25\3\26\3\26\7\26\u00a1\n\26\f\26\16\26\u00a4\13\26"+
		"\3\26\5\26\u00a7\n\26\3\26\6\26\u00aa\n\26\r\26\16\26\u00ab\3\26\7\26"+
		"\u00af\n\26\f\26\16\26\u00b2\13\26\3\26\5\26\u00b5\n\26\3\26\6\26\u00b8"+
		"\n\26\r\26\16\26\u00b9\5\26\u00bc\n\26\3\27\3\27\3\27\3\27\3\30\3\30\7"+
		"\30\u00c4\n\30\f\30\16\30\u00c7\13\30\3\30\3\30\2\31\3\3\1\5\4\1\7\5\1"+
		"\t\6\1\13\7\1\r\b\1\17\t\1\21\n\1\23\13\1\25\f\1\27\r\1\31\16\1\33\17"+
		"\1\35\20\1\37\21\1!\22\1#\23\1%\24\1\'\25\1)\26\1+\27\1-\30\2/\31\3\3"+
		"\2\n\4\2C\\c|\3\2\62;\13\2##%(,,\60\60B\\^^`ac|\u0082\u0101\3\2$$\5\2"+
		"\f\f\17\17$$\5\2\f\f\17\17))\7\2\13\f\17\17\"\"..^^\4\2\f\f\17\17\u00dd"+
		"\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2"+
		"\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2"+
		"\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2"+
		"\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2"+
		"\2\3\61\3\2\2\2\5\63\3\2\2\2\7\65\3\2\2\2\t\67\3\2\2\2\139\3\2\2\2\r;"+
		"\3\2\2\2\17?\3\2\2\2\21C\3\2\2\2\23G\3\2\2\2\25K\3\2\2\2\27P\3\2\2\2\31"+
		"S\3\2\2\2\33W\3\2\2\2\35\\\3\2\2\2\37`\3\2\2\2!b\3\2\2\2#e\3\2\2\2%\u0081"+
		"\3\2\2\2\'\u0083\3\2\2\2)\u009c\3\2\2\2+\u00bb\3\2\2\2-\u00bd\3\2\2\2"+
		"/\u00c1\3\2\2\2\61\62\7`\2\2\62\4\3\2\2\2\63\64\7\60\2\2\64\6\3\2\2\2"+
		"\65\66\7+\2\2\66\b\3\2\2\2\678\7.\2\28\n\3\2\2\29:\7*\2\2:\f\3\2\2\2;"+
		"<\7/\2\2<=\7/\2\2=>\7@\2\2>\16\3\2\2\2?@\7c\2\2@A\7p\2\2AB\7{\2\2B\20"+
		"\3\2\2\2CD\7c\2\2DE\7n\2\2EF\7n\2\2F\22\3\2\2\2GH\7v\2\2HI\7j\2\2IJ\7"+
		"g\2\2J\24\3\2\2\2KL\7v\2\2LM\7j\2\2MN\7k\2\2NO\7u\2\2O\26\3\2\2\2PQ\7"+
		"c\2\2QR\7p\2\2R\30\3\2\2\2ST\7f\2\2TU\7g\2\2UV\7h\2\2V\32\3\2\2\2WX\7"+
		"i\2\2XY\7g\2\2YZ\7v\2\2Z\34\3\2\2\2[]\t\2\2\2\\[\3\2\2\2]^\3\2\2\2^\\"+
		"\3\2\2\2^_\3\2\2\2_\36\3\2\2\2`a\t\2\2\2a \3\2\2\2bc\t\3\2\2c\"\3\2\2"+
		"\2df\7/\2\2ed\3\2\2\2ef\3\2\2\2fh\3\2\2\2gi\4\62;\2hg\3\2\2\2ij\3\2\2"+
		"\2jh\3\2\2\2jk\3\2\2\2kr\3\2\2\2ln\7\60\2\2mo\4\62;\2nm\3\2\2\2op\3\2"+
		"\2\2pn\3\2\2\2pq\3\2\2\2qs\3\2\2\2rl\3\2\2\2rs\3\2\2\2s\177\3\2\2\2tu"+
		"\7g\2\2uv\7G\2\2vx\3\2\2\2wy\7/\2\2xw\3\2\2\2xy\3\2\2\2y{\3\2\2\2z|\4"+
		"\62;\2{z\3\2\2\2|}\3\2\2\2}{\3\2\2\2}~\3\2\2\2~\u0080\3\2\2\2\177t\3\2"+
		"\2\2\177\u0080\3\2\2\2\u0080$\3\2\2\2\u0081\u0082\t\4\2\2\u0082&\3\2\2"+
		"\2\u0083\u0087\7$\2\2\u0084\u0086\n\5\2\2\u0085\u0084\3\2\2\2\u0086\u0089"+
		"\3\2\2\2\u0087\u0085\3\2\2\2\u0087\u0088\3\2\2\2\u0088\u008a\3\2\2\2\u0089"+
		"\u0087\3\2\2\2\u008a\u008b\7$\2\2\u008b(\3\2\2\2\u008c\u0090\7$\2\2\u008d"+
		"\u008f\n\6\2\2\u008e\u008d\3\2\2\2\u008f\u0092\3\2\2\2\u0090\u008e\3\2"+
		"\2\2\u0090\u0091\3\2\2\2\u0091\u0093\3\2\2\2\u0092\u0090\3\2\2\2\u0093"+
		"\u009d\7$\2\2\u0094\u0098\7)\2\2\u0095\u0097\n\7\2\2\u0096\u0095\3\2\2"+
		"\2\u0097\u009a\3\2\2\2\u0098\u0096\3\2\2\2\u0098\u0099\3\2\2\2\u0099\u009b"+
		"\3\2\2\2\u009a\u0098\3\2\2\2\u009b\u009d\7)\2\2\u009c\u008c\3\2\2\2\u009c"+
		"\u0094\3\2\2\2\u009d*\3\2\2\2\u009e\u00a6\7/\2\2\u009f\u00a1\4\62;\2\u00a0"+
		"\u009f\3\2\2\2\u00a1\u00a4\3\2\2\2\u00a2\u00a0\3\2\2\2\u00a2\u00a3\3\2"+
		"\2\2\u00a3\u00a5\3\2\2\2\u00a4\u00a2\3\2\2\2\u00a5\u00a7\7\60\2\2\u00a6"+
		"\u00a2\3\2\2\2\u00a6\u00a7\3\2\2\2\u00a7\u00a9\3\2\2\2\u00a8\u00aa\4\62"+
		";\2\u00a9\u00a8\3\2\2\2\u00aa\u00ab\3\2\2\2\u00ab\u00a9\3\2\2\2\u00ab"+
		"\u00ac\3\2\2\2\u00ac\u00bc\3\2\2\2\u00ad\u00af\4\62;\2\u00ae\u00ad\3\2"+
		"\2\2\u00af\u00b2\3\2\2\2\u00b0\u00ae\3\2\2\2\u00b0\u00b1\3\2\2\2\u00b1"+
		"\u00b3\3\2\2\2\u00b2\u00b0\3\2\2\2\u00b3\u00b5\7\60\2\2\u00b4\u00b0\3"+
		"\2\2\2\u00b4\u00b5\3\2\2\2\u00b5\u00b7\3\2\2\2\u00b6\u00b8\4\62;\2\u00b7"+
		"\u00b6\3\2\2\2\u00b8\u00b9\3\2\2\2\u00b9\u00b7\3\2\2\2\u00b9\u00ba\3\2"+
		"\2\2\u00ba\u00bc\3\2\2\2\u00bb\u009e\3\2\2\2\u00bb\u00b4\3\2\2\2\u00bc"+
		",\3\2\2\2\u00bd\u00be\t\b\2\2\u00be\u00bf\3\2\2\2\u00bf\u00c0\b\27\2\2"+
		"\u00c0.\3\2\2\2\u00c1\u00c5\7=\2\2\u00c2\u00c4\n\t\2\2\u00c3\u00c2\3\2"+
		"\2\2\u00c4\u00c7\3\2\2\2\u00c5\u00c3\3\2\2\2\u00c5\u00c6\3\2\2\2\u00c6"+
		"\u00c8\3\2\2\2\u00c7\u00c5\3\2\2\2\u00c8\u00c9\b\30\3\2\u00c9\60\3\2\2"+
		"\2\27\2^ejprx}\177\u0087\u0090\u0098\u009c\u00a2\u00a6\u00ab\u00b0\u00b4"+
		"\u00b9\u00bb\u00c5";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}