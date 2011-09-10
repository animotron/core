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
package org.animotron.graph.builder;

import org.animotron.exception.AnimoException;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Operator;

import java.io.*;
import java.util.Stack;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoBuilder extends GraphBuilder {

    private Reader reader;
    private StringBuilder s = new StringBuilder();
    private int level = 0;
    private Stack<Integer> stack = new Stack<Integer>();

    public AnimoBuilder (InputStream stream) {
        reader = new InputStreamReader(stream);
    }

    public AnimoBuilder (String str) {
        reader = new StringReader(str);
    }

    public void build() throws IOException, AnimoException {

        int len;
        char[] buff = new char[4 * 1024];

        boolean text = false;
        char prev = '\0';

        startGraph();
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
                            case '('  : startList();
                                        newToken(s, text);
                                        break;
                            case ')'  : newToken(s, text);
                                        endList();
                                        break;
                            default   : s.append(ch);
                        }
                    }
                }
                prev = prev == '\\' ? '\0' : ch;
            }
            lastToken(s, text);
        }

        endList();
        endGraph();

    }

    private void newToken(StringBuilder s, boolean text) {
        if (s.length() > 0) {
            token(s.toString(), text);
            this.s = new StringBuilder();
        }
    }

    private void lastToken(StringBuilder s, boolean text) {
        if (s.length() > 0) {
            token(s.toString(), text);
        }
    }

    Statement op = null;
    private void token(String token, boolean text) {
        if (text) {
            start(token);
            end();
            op = null;
        } else {
            if (op instanceof Operator) {
                start(op, token);
                op = null;
                level++;
            } else {
                op = Statements.name(token);
                if (!(op instanceof Operator)) {
                    start(AN._, token);
                    level ++;
                }
            }
        }
    }

    private void startList() {
        stack.push(level);
        level = 0;
    }

    private void endList() {
        for (int i = 0; i < level; i++) {
            end();
        }
        level = stack.pop();
    }

}
