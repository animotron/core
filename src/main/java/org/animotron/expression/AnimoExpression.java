/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.expression;

import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.LINK;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.ml.*;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.relation.Relation;

import java.io.*;
import java.util.Stack;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoExpression extends Expression {

    private Reader reader;

    public AnimoExpression(InputStream stream) throws IOException {
        this(new InputStreamReader(stream));
    }

    public AnimoExpression(GraphBuilder builder, InputStream stream) throws IOException {
        this(builder, new InputStreamReader(stream));
    }

    public AnimoExpression(String str) throws IOException {
        this(new StringReader(str));
    }

    public AnimoExpression(GraphBuilder builder, String str) throws IOException {
        this(builder, new StringReader(str));
    }

    public AnimoExpression(Reader reader) throws IOException {
        this(new FastGraphBuilder(), reader);
    }

    public AnimoExpression(GraphBuilder builder, Reader reader) throws IOException {
        super(builder);
        this.reader = reader;
        builder.build(this);
    }

    private StringBuilder s = new StringBuilder();
    private Stack<Integer> stack = new Stack<Integer>();
    private Statement op = null;
    private int level = 0;
    private boolean prefix = false;
    boolean link = false;

    @Override
    public void build() throws IOException, AnimoException {
        int len;
        char[] buff = new char[4 * 1024];

        boolean text = false;
        char prev = '\0';

        builder.startGraph();
        startList();

        while ((len=reader.read(buff))>0) {
            for (int i = 0; i < len; i++) {
                char ch = buff[i];
                if (ch == '\"') {
                    if (!text) {
                        newToken(s, text);
                        text = true;
                    } else if (prev == '\\') {
                        s.append(ch);
                    } else {
                        newToken(s, text);
                        text = false;
                    }
                } else {
                    if (text) {
                        if (prev == '\\' || ch != '\\') {
                            s.append(ch);
                        }
                    } else {
                        switch (ch) {
                            case ' '  :
                            case '\t' :
                            case '\n' : newToken(s, text);
                                        break;
                            case '('  : newToken(s, text);
                                        startList();
                                        break;
                            case ')'  : newToken(s, text);
                                        endList();
                                        break;
                            default   : s.append(ch);
                                        Statement st = Statements.name(s.toString());
                                        if (st instanceof Prefix) {
                                            builder.start(st);
                                            level++;
                                            op = st;
                                            link = false;
                                            prefix = true;
                                            s = new StringBuilder();
                                        }
                        }
                    }
                }
                prev = prev == '\\' ? '\0' : ch;
            }
            lastToken(s, text);
        }
        endList();
        builder.endGraph();
    }

    private void newToken(StringBuilder s, boolean text) throws AnimoException {
        if (s.length() > 0) {
            token(s.toString(), text);
            this.s = new StringBuilder();
        } else {
            prefix = false;
        }
    }

    private void lastToken(StringBuilder s, boolean text) throws AnimoException {
        if (s.length() > 0) {
            token(s.toString(), text);
        } else {
            prefix = false;
        }
    }

    private void token(String token, boolean text) throws AnimoException {
        if (text) {
            if (op instanceof Prefix && !(op instanceof NS) && !link) {
                builder.start(NAME._, token);
                builder.end();
            } else {
                builder.start(TEXT._, token);
                level++;
            }
        } else {
            if (prefix) {
                builder.start(NAME._, token);
                builder.end();
                op = null;
                prefix = false;
            } else if (op instanceof Operator || op instanceof Relation) {
                builder.start(op, token);
                op = null;
                level++;
            } else {
                op = Statements.name(token);
                if (op instanceof MLOperator || op instanceof LINK) {
                    builder.start(op);
                    level++;
                } else if (op == null || op instanceof Instruction) {
                    builder.start(AN._, token);
                    level++;
                }
            }
        }
        link = false;
    }

    private void startList() throws AnimoException {
        if (link) {
            builder.start(LINK._);
            op = null;
            level++;
        } else {
            link = true;
        }
        stack.push(level);
        level = 0;
    }

    private void endList() {
        for (int i = 0; i < level; i++) {
            builder.end();
        }
        level = stack.pop();
    }

}
