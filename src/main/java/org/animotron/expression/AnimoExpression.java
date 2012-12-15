/*
 *  Copyright (C) 2011-2012 The Animo Project
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
package org.animotron.expression;

import org.animotron.exception.AnimoException;
import org.animotron.statement.Prefix;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.link.AbstractLink;
import org.animotron.statement.link.LINK;
import org.animotron.statement.operator.*;
import org.animotron.statement.operator.VALUE;
import org.neo4j.graphdb.Relationship;

import java.io.*;
import java.util.Stack;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoExpression extends Expression {

    public final static Relationship[] __(String... e) {
        Relationship[] a = new Relationship[e.length];
        for (int i = 0; i < e.length; i++) {
        	System.out.println(e[i]);
            a[i] = __(new AnimoExpression(e[i]));
        }
        return a;
    }

	private Reader reader;
    private boolean comma = false;

    public AnimoExpression(InputStream stream) {
        this(new InputStreamReader(stream));
    }

    public AnimoExpression(String str) {
        this(new StringReader(str));
    }

    public AnimoExpression(Reader reader) {
        this.reader = reader;
    }

    private StringBuilder s = new StringBuilder();
    private Stack<Integer> stack = new Stack<Integer>();
    private Statement op = null;
    private int level = 0;
    boolean link = false;
    boolean link_ = false;
    boolean number = true;
    boolean text = false;
    char quote = '\0';
    int para = 0;

    @Override
    public void build() throws IOException, AnimoException {
        int len;
        char[] buff = new char[4 * 1024];

        char prev = '\0';

        startList();

        while ((len=reader.read(buff))>0) {
            for (int i = 0; i < len; i++) {
                char ch = buff[i];
                if (ch == '\"' || ch == '\'') {
                    if (!text) {
                        newToken();
                        text = true;
                        quote = ch;
                        para++;
                    } else if (prev == '\\') {
                        s.append(ch);
                    } else if (quote == ch) {
                        newToken();
                        text = false;
                        para--;
                    } else {
                        s.append(ch);
                    }
                } else {
                    if (text) {
                        if (prev == '\\' || ch != '\\') {
                            s.append(ch);
                        }
                    } else {
                    	if ((ch != ' ' && ch != '\t' && ch != '\n' && ch != ',' && ch != '(' && ch != ')') || (ch == '.' && number)) {
                            if (ch != '.') {
                                if (prev == '.') {
                                    s.append('.');
                                }
                                s.append(ch);
                                processPrefix();
                            }
                    	} else {
                    		number = false;
	                        switch (ch) {
	                            case ' '  :
	                            case '.'  : //workaround
	                            case '\t' :
	                            case '\n' : newToken();
	                                        break;
	                            case ','  : newToken();
	                                        comma = true;
	                                        break;
	                            case '('  : newToken();
	                                        startList();
                                            link_ = true;
                                            para++;
	                                        break;
	                            case ')'  : link_ = false;
                                            newToken();
	                                        endList();
                                            para--;
	                                        break;
	                            default   : s.append(ch);
                                            processPrefix();
	                        }
                    	}
                    }
                }
                prev = prev == '\\' ? '\0' : ch;
            }
        }
        lastToken();
        endList();
        if (para != 0) {
            throw new AnimoException((Relationship) null);
        }
    }

    private void processPrefix() throws AnimoException, IOException {
        value();
        Statement st = Statements.name(s.toString());
        if (st instanceof Prefix) {
            builder.start(st);
            level++;
            op = st;
            link = false;
            s = new StringBuilder();
            number = true;
        }
    }

    private void newToken() throws AnimoException, IOException {
        if (s.length() > 0) {
            token();
            s = new StringBuilder();
        }
        number = true;
    }

    private void lastToken() throws AnimoException, IOException {
        if (s.length() > 0) {
            token();
        }
        number = true;
    }

    private Object v = null;

    private void value(Object o) throws AnimoException, IOException {
        if (link_) {
            v = o;
        } else {
            builder._(o);
        }
    }

    private void value() throws AnimoException, IOException {
        if (v != null) {
            if (link_) {
                builder.start(LINK._);
                link_ = false;
                level++;
            }
            builder._(v);
            v = null;
        }
    }

    private void token() throws AnimoException, IOException {
        value();
        String token = s.toString();
    	if (token.length() == 1 && ".".equals(token) && !text) return; //XXX:start new graph
    	if (text) {
            value(token);
        } else {
            if (op instanceof DEF) {
                builder.start(op, token);
                op = null;
                level++;
            } else if (token.endsWith(POSSESSIVE._.name())) {
                token = token.substring(0, token.length()-POSSESSIVE._.name().length());
                builder.start(POSSESSIVE._);
                builder._(REF._, token);
                op = null;
                level++;
            } else {
                Statement s = Statements.name(token);
                if (s instanceof Operator) {
                    builder.start(s);
                    level++;
                } else if (s instanceof AbstractLink) {
                    builder.start(s);
                    s = null;
                    level++;
                } else if (s instanceof Instruction) {
                    builder.start(AN._);
                    builder._(REF._, token);
                    level++;
                } else if (s == null) {
                    Object o = VALUE.value(token);
                    if (o instanceof String) {
                        if (op instanceof REF && !comma || !(op instanceof Operator || op instanceof REF)) {
                            builder.start(AN._);
                            level++;
                        }
                        s =  REF._;
                        builder._(s, token);
                        comma = false;
                    } else {
                        value(o);
                    }
                }
                op = s;
            }
        }

        link = false;
        if (v == null) {
            link_ = false;
        }

    }

    private void startList() throws AnimoException, IOException {
        value();
        if (link) {
            builder.start(LINK._);
            op = null;
            level++;
        } else {
            link = true;
        }
        stack.push(level);
        level = 0;
        op = null;
    }

    private void endList() throws AnimoException, IOException {
        value();
        for (int i = 0; i < level; i++) {
            builder.end();
        }
        level = stack.pop();
        op = null;
    }

}
