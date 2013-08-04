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
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link AnimoGrammParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface AnimoGrammVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#many}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMany(@NotNull AnimoGrammParser.ManyContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#point}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPoint(@NotNull AnimoGrammParser.PointContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#prog}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProg(@NotNull AnimoGrammParser.ProgContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#str}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStr(@NotNull AnimoGrammParser.StrContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#def}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDef(@NotNull AnimoGrammParser.DefContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#link2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLink2(@NotNull AnimoGrammParser.Link2Context ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#name}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitName(@NotNull AnimoGrammParser.NameContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#link}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLink(@NotNull AnimoGrammParser.LinkContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#context}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContext(@NotNull AnimoGrammParser.ContextContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#action}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAction(@NotNull AnimoGrammParser.ActionContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#value}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValue(@NotNull AnimoGrammParser.ValueContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#usage}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUsage(@NotNull AnimoGrammParser.UsageContext ctx);

	/**
	 * Visit a parse tree produced by {@link AnimoGrammParser#number_val}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumber_val(@NotNull AnimoGrammParser.Number_valContext ctx);
}