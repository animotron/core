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

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNSimulator;
import org.antlr.v4.runtime.atn.ParserATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AnimoGrammParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__4=1, T__3=2, T__2=3, T__1=4, T__0=5, LINK=6, ANY=7, ALL=8, THE=9, THIS=10, 
		AN=11, DEF=12, GET=13, WORD=14, NAME_LETTER=15, NUMER=16, NUMBER=17, LETTER=18, 
		STR=19, STRING=20, NUM=21, WS=22, COMMENT=23;
	public static final String[] tokenNames = {
		"<INVALID>", "'^'", "'.'", "')'", "','", "'('", "'-->'", "'any'", "'all'", 
		"'the'", "'this'", "'an'", "'def'", "'get'", "WORD", "NAME_LETTER", "NUMER", 
		"NUMBER", "LETTER", "STR", "STRING", "NUM", "WS", "COMMENT"
	};
	public static final int
		RULE_prog = 0, RULE_def = 1, RULE_value = 2, RULE_usage = 3, RULE_action = 4, 
		RULE_many = 5, RULE_point = 6, RULE_context = 7, RULE_link = 8, RULE_link2 = 9, 
		RULE_name = 10, RULE_str = 11, RULE_number_val = 12;
	public static final String[] ruleNames = {
		"prog", "def", "value", "usage", "action", "many", "point", "context", 
		"link", "link2", "name", "str", "number_val"
	};

	@Override
	public String getGrammarFileName() { return "AnimoGramm.g"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public AnimoGrammParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class ProgContext extends ParserRuleContext {
		public List<TerminalNode> DEF() { return getTokens(AnimoGrammParser.DEF); }
		public TerminalNode DEF(int i) {
			return getToken(AnimoGrammParser.DEF, i);
		}
		public List<DefContext> def() {
			return getRuleContexts(DefContext.class);
		}
		public DefContext def(int i) {
			return getRuleContext(DefContext.class,i);
		}
		public ProgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_prog; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterProg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitProg(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitProg(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgContext prog() throws RecognitionException {
		ProgContext _localctx = new ProgContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_prog);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(30); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(26); match(DEF);
				setState(27); def();
				setState(28); match(2);
				}
				}
				setState(32); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==DEF );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DefContext extends ParserRuleContext {
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public UsageContext usage(int i) {
			return getRuleContext(UsageContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public PointContext point() {
			return getRuleContext(PointContext.class,0);
		}
		public ManyContext many() {
			return getRuleContext(ManyContext.class,0);
		}
		public List<UsageContext> usage() {
			return getRuleContexts(UsageContext.class);
		}
		public ActionContext action() {
			return getRuleContext(ActionContext.class,0);
		}
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public DefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_def; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DefContext def() throws RecognitionException {
		DefContext _localctx = new DefContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_def);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(43);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				{
				setState(34); action();
				}
				break;

			case 2:
				{
				setState(35); many();
				}
				break;

			case 3:
				{
				setState(36); point();
				}
				break;

			case 4:
				{
				setState(37); action();
				setState(38); many();
				}
				break;

			case 5:
				{
				setState(40); many();
				setState(41); point();
				}
				break;
			}
			setState(45); name();
			setState(52);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << 4) | (1L << 5) | (1L << ANY) | (1L << ALL) | (1L << THE) | (1L << THIS) | (1L << AN) | (1L << GET) | (1L << WORD) | (1L << NUMER) | (1L << STR))) != 0)) {
				{
				setState(50);
				switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
				case 1:
					{
					setState(46); usage();
					}
					break;

				case 2:
					{
					setState(47); value();
					}
					break;

				case 3:
					{
					setState(48); match(4);
					setState(49); value();
					}
					break;
				}
				}
				setState(54);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ValueContext extends ParserRuleContext {
		public PointContext point() {
			return getRuleContext(PointContext.class,0);
		}
		public ManyContext many() {
			return getRuleContext(ManyContext.class,0);
		}
		public Number_valContext number_val() {
			return getRuleContext(Number_valContext.class,0);
		}
		public StrContext str() {
			return getRuleContext(StrContext.class,0);
		}
		public ActionContext action() {
			return getRuleContext(ActionContext.class,0);
		}
		public UsageContext usage() {
			return getRuleContext(UsageContext.class,0);
		}
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public ValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_value; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitValue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValueContext value() throws RecognitionException {
		ValueContext _localctx = new ValueContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_value);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(64);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				{
				setState(55); action();
				}
				break;

			case 2:
				{
				setState(56); many();
				}
				break;

			case 3:
				{
				setState(57); point();
				}
				break;

			case 4:
				{
				setState(58); action();
				setState(59); many();
				}
				break;

			case 5:
				{
				setState(61); many();
				setState(62); point();
				}
				break;
			}
			setState(70);
			switch (_input.LA(1)) {
			case NUMER:
				{
				setState(66); number_val();
				}
				break;
			case WORD:
				{
				setState(67); name();
				}
				break;
			case STR:
				{
				setState(68); str();
				}
				break;
			case 5:
			case ANY:
			case ALL:
				{
				setState(69); usage();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UsageContext extends ParserRuleContext {
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public PointContext point() {
			return getRuleContext(PointContext.class,0);
		}
		public List<ManyContext> many() {
			return getRuleContexts(ManyContext.class);
		}
		public ContextContext context() {
			return getRuleContext(ContextContext.class,0);
		}
		public Link2Context link2() {
			return getRuleContext(Link2Context.class,0);
		}
		public LinkContext link() {
			return getRuleContext(LinkContext.class,0);
		}
		public ManyContext many(int i) {
			return getRuleContext(ManyContext.class,i);
		}
		public UsageContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_usage; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterUsage(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitUsage(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitUsage(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UsageContext usage() throws RecognitionException {
		UsageContext _localctx = new UsageContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_usage);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(73);
			_la = _input.LA(1);
			if (_la==ANY || _la==ALL) {
				{
				setState(72); many();
				}
			}

			setState(75); match(5);
			setState(87);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				{
				setState(76); link();
				}
				break;

			case 2:
				{
				setState(77); link2();
				}
				break;

			case 3:
				{
				setState(78); context();
				}
				break;

			case 4:
				{
				setState(80); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(79); value();
					}
					}
					setState(82); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << 5) | (1L << ANY) | (1L << ALL) | (1L << THE) | (1L << THIS) | (1L << AN) | (1L << GET) | (1L << WORD) | (1L << NUMER) | (1L << STR))) != 0) );
				}
				break;

			case 5:
				{
				setState(84); many();
				setState(85); point();
				}
				break;
			}
			setState(89); match(3);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ActionContext extends ParserRuleContext {
		public TerminalNode GET() { return getToken(AnimoGrammParser.GET, 0); }
		public TerminalNode AN() { return getToken(AnimoGrammParser.AN, 0); }
		public ActionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_action; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterAction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitAction(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitAction(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ActionContext action() throws RecognitionException {
		ActionContext _localctx = new ActionContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_action);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(91);
			_la = _input.LA(1);
			if ( !(_la==AN || _la==GET) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ManyContext extends ParserRuleContext {
		public TerminalNode ALL() { return getToken(AnimoGrammParser.ALL, 0); }
		public TerminalNode ANY() { return getToken(AnimoGrammParser.ANY, 0); }
		public ManyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_many; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterMany(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitMany(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitMany(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ManyContext many() throws RecognitionException {
		ManyContext _localctx = new ManyContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_many);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(93);
			_la = _input.LA(1);
			if ( !(_la==ANY || _la==ALL) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PointContext extends ParserRuleContext {
		public TerminalNode THE() { return getToken(AnimoGrammParser.THE, 0); }
		public TerminalNode THIS() { return getToken(AnimoGrammParser.THIS, 0); }
		public PointContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_point; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterPoint(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitPoint(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitPoint(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PointContext point() throws RecognitionException {
		PointContext _localctx = new PointContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_point);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(95);
			_la = _input.LA(1);
			if ( !(_la==THE || _la==THIS) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ContextContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public ContextContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_context; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterContext(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitContext(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitContext(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ContextContext context() throws RecognitionException {
		ContextContext _localctx = new ContextContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_context);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(97); match(1);
			setState(98); name();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LinkContext extends ParserRuleContext {
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public TerminalNode LINK() { return getToken(AnimoGrammParser.LINK, 0); }
		public LinkContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_link; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterLink(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitLink(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitLink(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LinkContext link() throws RecognitionException {
		LinkContext _localctx = new LinkContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_link);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(100); match(LINK);
			setState(102); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(101); value();
				}
				}
				setState(104); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << 5) | (1L << ANY) | (1L << ALL) | (1L << THE) | (1L << THIS) | (1L << AN) | (1L << GET) | (1L << WORD) | (1L << NUMER) | (1L << STR))) != 0) );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Link2Context extends ParserRuleContext {
		public TerminalNode LINK(int i) {
			return getToken(AnimoGrammParser.LINK, i);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public List<TerminalNode> LINK() { return getTokens(AnimoGrammParser.LINK); }
		public Link2Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_link2; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterLink2(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitLink2(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitLink2(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Link2Context link2() throws RecognitionException {
		Link2Context _localctx = new Link2Context(_ctx, getState());
		enterRule(_localctx, 18, RULE_link2);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(106); match(LINK);
			setState(107); match(LINK);
			setState(109); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(108); value();
				}
				}
				setState(111); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << 5) | (1L << ANY) | (1L << ALL) | (1L << THE) | (1L << THIS) | (1L << AN) | (1L << GET) | (1L << WORD) | (1L << NUMER) | (1L << STR))) != 0) );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NameContext extends ParserRuleContext {
		public TerminalNode WORD() { return getToken(AnimoGrammParser.WORD, 0); }
		public NameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_name; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NameContext name() throws RecognitionException {
		NameContext _localctx = new NameContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_name);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(113); match(WORD);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StrContext extends ParserRuleContext {
		public TerminalNode STR() { return getToken(AnimoGrammParser.STR, 0); }
		public StrContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_str; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterStr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitStr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitStr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StrContext str() throws RecognitionException {
		StrContext _localctx = new StrContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_str);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(115); match(STR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Number_valContext extends ParserRuleContext {
		public TerminalNode NUMER() { return getToken(AnimoGrammParser.NUMER, 0); }
		public Number_valContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_number_val; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).enterNumber_val(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnimoGrammListener ) ((AnimoGrammListener)listener).exitNumber_val(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnimoGrammVisitor ) return ((AnimoGrammVisitor<? extends T>)visitor).visitNumber_val(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Number_valContext number_val() throws RecognitionException {
		Number_valContext _localctx = new Number_valContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_number_val);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(117); match(NUMER);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3\31z\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4"+
		"\f\t\f\4\r\t\r\4\16\t\16\3\2\3\2\3\2\3\2\6\2!\n\2\r\2\16\2\"\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3.\n\3\3\3\3\3\3\3\3\3\3\3\7\3\65\n\3\f\3"+
		"\16\38\13\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\5\4C\n\4\3\4\3\4\3\4\3"+
		"\4\5\4I\n\4\3\5\5\5L\n\5\3\5\3\5\3\5\3\5\3\5\6\5S\n\5\r\5\16\5T\3\5\3"+
		"\5\3\5\5\5Z\n\5\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\t\3\n\3\n\6"+
		"\ni\n\n\r\n\16\nj\3\13\3\13\3\13\6\13p\n\13\r\13\16\13q\3\f\3\f\3\r\3"+
		"\r\3\16\3\16\3\16\2\17\2\4\6\b\n\f\16\20\22\24\26\30\32\2\5\4\2\r\r\17"+
		"\17\3\2\t\n\3\2\13\f\u0085\2 \3\2\2\2\4-\3\2\2\2\6B\3\2\2\2\bK\3\2\2\2"+
		"\n]\3\2\2\2\f_\3\2\2\2\16a\3\2\2\2\20c\3\2\2\2\22f\3\2\2\2\24l\3\2\2\2"+
		"\26s\3\2\2\2\30u\3\2\2\2\32w\3\2\2\2\34\35\7\16\2\2\35\36\5\4\3\2\36\37"+
		"\7\4\2\2\37!\3\2\2\2 \34\3\2\2\2!\"\3\2\2\2\" \3\2\2\2\"#\3\2\2\2#\3\3"+
		"\2\2\2$.\5\n\6\2%.\5\f\7\2&.\5\16\b\2\'(\5\n\6\2()\5\f\7\2).\3\2\2\2*"+
		"+\5\f\7\2+,\5\16\b\2,.\3\2\2\2-$\3\2\2\2-%\3\2\2\2-&\3\2\2\2-\'\3\2\2"+
		"\2-*\3\2\2\2-.\3\2\2\2./\3\2\2\2/\66\5\26\f\2\60\65\5\b\5\2\61\65\5\6"+
		"\4\2\62\63\7\6\2\2\63\65\5\6\4\2\64\60\3\2\2\2\64\61\3\2\2\2\64\62\3\2"+
		"\2\2\658\3\2\2\2\66\64\3\2\2\2\66\67\3\2\2\2\67\5\3\2\2\28\66\3\2\2\2"+
		"9C\5\n\6\2:C\5\f\7\2;C\5\16\b\2<=\5\n\6\2=>\5\f\7\2>C\3\2\2\2?@\5\f\7"+
		"\2@A\5\16\b\2AC\3\2\2\2B9\3\2\2\2B:\3\2\2\2B;\3\2\2\2B<\3\2\2\2B?\3\2"+
		"\2\2BC\3\2\2\2CH\3\2\2\2DI\5\32\16\2EI\5\26\f\2FI\5\30\r\2GI\5\b\5\2H"+
		"D\3\2\2\2HE\3\2\2\2HF\3\2\2\2HG\3\2\2\2I\7\3\2\2\2JL\5\f\7\2KJ\3\2\2\2"+
		"KL\3\2\2\2LM\3\2\2\2MY\7\7\2\2NZ\5\22\n\2OZ\5\24\13\2PZ\5\20\t\2QS\5\6"+
		"\4\2RQ\3\2\2\2ST\3\2\2\2TR\3\2\2\2TU\3\2\2\2UZ\3\2\2\2VW\5\f\7\2WX\5\16"+
		"\b\2XZ\3\2\2\2YN\3\2\2\2YO\3\2\2\2YP\3\2\2\2YR\3\2\2\2YV\3\2\2\2Z[\3\2"+
		"\2\2[\\\7\5\2\2\\\t\3\2\2\2]^\t\2\2\2^\13\3\2\2\2_`\t\3\2\2`\r\3\2\2\2"+
		"ab\t\4\2\2b\17\3\2\2\2cd\7\3\2\2de\5\26\f\2e\21\3\2\2\2fh\7\b\2\2gi\5"+
		"\6\4\2hg\3\2\2\2ij\3\2\2\2jh\3\2\2\2jk\3\2\2\2k\23\3\2\2\2lm\7\b\2\2m"+
		"o\7\b\2\2np\5\6\4\2on\3\2\2\2pq\3\2\2\2qo\3\2\2\2qr\3\2\2\2r\25\3\2\2"+
		"\2st\7\20\2\2t\27\3\2\2\2uv\7\25\2\2v\31\3\2\2\2wx\7\22\2\2x\33\3\2\2"+
		"\2\r\"-\64\66BHKTYjq";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}