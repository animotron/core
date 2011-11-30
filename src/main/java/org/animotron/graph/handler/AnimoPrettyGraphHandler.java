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
package org.animotron.graph.handler;

import org.animotron.statement.Prefix;
import org.animotron.statement.Statement;
import org.animotron.statement.Suffix;
import org.animotron.statement.link.LINK;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.operator.*;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoPrettyGraphHandler extends AnimoGraphHandler {

    private Object[] root;
    private List<Object[]> list;
    private Stack<Object[]> stack;
    private final static String INDENT = "    ";

    public AnimoPrettyGraphHandler(OutputStream stream) {
        super (stream);
    }

    public AnimoPrettyGraphHandler(StringBuilder builder) {
        super(builder);
    }

    @Override
    public void startGraph() {
        stack = new Stack<Object[]>();
        list = new LinkedList<Object[]>();
    }

    @Override
    public void start(Statement statement, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        Object[] item = {statement, r, level, isOne, new LinkedList<Object[]>(), !isOne, pos};
        if (!stack.empty()) {
            Object[]p = stack.peek();
            ((List<Object[]>) p[4]).add(item);
        }
        stack.push(item);
    }

    @Override
    public void end(Statement statement, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        root = stack.pop();
        int size = ((List<Object[]>)root[4]).size();
        //if (statement instanceof Prefix) size--;
        if (!stack.empty()) {
            if (stack.peek()[0] instanceof Prefix) size--;
        } else {
            list.add(root);
        }
        root[5] = !(statement instanceof REF || statement instanceof AN && pos == 0) && ((Boolean)root[5] || size > 1) || (level == 1 && statement instanceof THE);
    }

    @Override
    public void endGraph() throws IOException {
        for (Object[] o : list) {
            write(o, 0);
            write("\n");
        }
    }

    Statement ps = null;
    private void write(Object[] o, int indent) throws IOException {
        Statement statement = (Statement) o[0];
        int level = (Integer) o[2];
        boolean isOne = (Boolean) o[3];
        if (statement instanceof AN) {
            if (level != 0) {
                if (!(ps instanceof LINK)) {
            		if ((Boolean) o[5] || ps instanceof THE) {
            			indent++;
                		write("\n");
            			for (int i = 0; i < indent; i++) {
            				write(INDENT);
            			}
        			} else {
        			    write(" ");
        			}
                }
                if (!isOne) {
                    write("(");
                }
                Node n= ((Relationship) o[1]).getEndNode();
                if (!n.hasRelationship(REF._, OUTGOING)) {
                    write(statement.name());
                } else if (((Integer) o[6]) == 0 && ps instanceof Operator) {
                    write(statement.name());
                    if (n.hasRelationship(OUTGOING)) {
                        write(" ");
                    }
                }
            }
        } else {
            if (level != 0 && !(statement instanceof QNAME)) {
                if ((Boolean) o[5]) {
                    indent++;
                    write("\n");
                    for (int i = 0; i < indent; i++) {
                        write(INDENT);
                    }
                } else if (!(ps instanceof LINK || statement instanceof Suffix)) {
                    if (statement instanceof REF && ps instanceof REF) {
                        write(",");
                    } else {
                        if (!(statement instanceof REF && ps instanceof AN)) {
                        	if (statement instanceof Evaluable) {
	                            indent++;
	                            write("\n");
	                            for (int i = 0; i < indent; i++) {
	                                write(INDENT);
	                            }
                        	} else
                        		write(" ");
                        }
                    }
                }
                if (!(statement instanceof REF || statement instanceof VALUE) && (!isOne || statement instanceof LINK)) {
                    write("(");
                }
            }
            write(statement, (Relationship) o[1]);
        }
        ps = statement;
        for (Object[] i : (List<Object[]>) o[4]) {
            write(i, indent);
        }
        if (level == 0) {
            write(".");
        } else if (!(statement instanceof REF || statement instanceof QNAME || statement instanceof VALUE) && (!isOne || statement instanceof LINK)) {
            write(")");
        }
    }

}