/*
 *  Copyright (C) 2011-2013 The Animo Project
 *  http://animotron.org
 *
 *  This file is part of Animotron.
 *
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of
 *  the GNU Affero General Public License along with Animotron.
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.lang;

import org.animotron.exception.AnimoException;
import org.animotron.expression.Expression;
import org.animotron.lang.antlr.AnimoGrammBaseListener;
import org.animotron.lang.antlr.AnimoGrammLexer;
import org.animotron.lang.antlr.AnimoGrammParser;
import org.animotron.statement.link.LINK;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.misc.NotNull;

import java.io.*;

/**
 * Created with IntelliJ IDEA.
 * @author <a href="mailto:wstarcev@gmail.com">Vasilii Startsev</a>
 * Date: 04.08.13
 * Time: 9:12
 */
public class AnimoExpression extends Expression{

    private AnimoGrammParser parser = null;

    public AnimoExpression(InputStream stream) {
        this(new InputStreamReader(stream));
    }

    public AnimoExpression(String str) {
        this(new StringReader(str));
    }

    public AnimoExpression(Reader reader) {
        try {
            AnimoGrammLexer lexer = new AnimoGrammLexer(new ANTLRInputStream(reader));
            parser = new AnimoGrammParser(new CommonTokenStream(lexer));
            parser.addParseListener(new AnimoGrammBaseListener() {
                @Override
                public void enterDef(@NotNull AnimoGrammParser.DefContext ctx) {
                    try {
                        builder.start(LINK._);
                    } catch (AnimoException e) {
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    super.enterDef(ctx);
                }

                @Override
                public void exitDef(@NotNull AnimoGrammParser.DefContext ctx) {
                    try {
                        builder.end();
                    } catch (AnimoException e) {
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    super.exitDef(ctx);
                }

                @Override
                public void enterStr(@NotNull AnimoGrammParser.StrContext ctx) {
                    try {
                        builder._(ctx.getText());
                    } catch (AnimoException e) {
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    super.enterStr(ctx);
                }
            });
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    @Override
    public void build() throws Throwable {

    }
}
