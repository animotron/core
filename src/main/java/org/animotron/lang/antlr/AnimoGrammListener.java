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

import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link AnimoGrammParser}.
 */
public interface AnimoGrammListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#many}.
	 * @param ctx the parse tree
	 */
	void enterMany(@NotNull AnimoGrammParser.ManyContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#many}.
	 * @param ctx the parse tree
	 */
	void exitMany(@NotNull AnimoGrammParser.ManyContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#point}.
	 * @param ctx the parse tree
	 */
	void enterPoint(@NotNull AnimoGrammParser.PointContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#point}.
	 * @param ctx the parse tree
	 */
	void exitPoint(@NotNull AnimoGrammParser.PointContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#prog}.
	 * @param ctx the parse tree
	 */
	void enterProg(@NotNull AnimoGrammParser.ProgContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#prog}.
	 * @param ctx the parse tree
	 */
	void exitProg(@NotNull AnimoGrammParser.ProgContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#str}.
	 * @param ctx the parse tree
	 */
	void enterStr(@NotNull AnimoGrammParser.StrContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#str}.
	 * @param ctx the parse tree
	 */
	void exitStr(@NotNull AnimoGrammParser.StrContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#def}.
	 * @param ctx the parse tree
	 */
	void enterDef(@NotNull AnimoGrammParser.DefContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#def}.
	 * @param ctx the parse tree
	 */
	void exitDef(@NotNull AnimoGrammParser.DefContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#link2}.
	 * @param ctx the parse tree
	 */
	void enterLink2(@NotNull AnimoGrammParser.Link2Context ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#link2}.
	 * @param ctx the parse tree
	 */
	void exitLink2(@NotNull AnimoGrammParser.Link2Context ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#name}.
	 * @param ctx the parse tree
	 */
	void enterName(@NotNull AnimoGrammParser.NameContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#name}.
	 * @param ctx the parse tree
	 */
	void exitName(@NotNull AnimoGrammParser.NameContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#link}.
	 * @param ctx the parse tree
	 */
	void enterLink(@NotNull AnimoGrammParser.LinkContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#link}.
	 * @param ctx the parse tree
	 */
	void exitLink(@NotNull AnimoGrammParser.LinkContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#context}.
	 * @param ctx the parse tree
	 */
	void enterContext(@NotNull AnimoGrammParser.ContextContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#context}.
	 * @param ctx the parse tree
	 */
	void exitContext(@NotNull AnimoGrammParser.ContextContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#action}.
	 * @param ctx the parse tree
	 */
	void enterAction(@NotNull AnimoGrammParser.ActionContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#action}.
	 * @param ctx the parse tree
	 */
	void exitAction(@NotNull AnimoGrammParser.ActionContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#value}.
	 * @param ctx the parse tree
	 */
	void enterValue(@NotNull AnimoGrammParser.ValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#value}.
	 * @param ctx the parse tree
	 */
	void exitValue(@NotNull AnimoGrammParser.ValueContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#usage}.
	 * @param ctx the parse tree
	 */
	void enterUsage(@NotNull AnimoGrammParser.UsageContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#usage}.
	 * @param ctx the parse tree
	 */
	void exitUsage(@NotNull AnimoGrammParser.UsageContext ctx);

	/**
	 * Enter a parse tree produced by {@link AnimoGrammParser#number_val}.
	 * @param ctx the parse tree
	 */
	void enterNumber_val(@NotNull AnimoGrammParser.Number_valContext ctx);
	/**
	 * Exit a parse tree produced by {@link AnimoGrammParser#number_val}.
	 * @param ctx the parse tree
	 */
	void exitNumber_val(@NotNull AnimoGrammParser.Number_valContext ctx);
}