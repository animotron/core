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
import org.animotron.expression.BinaryExpression.StringArrayIterator;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.THE;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.regex.Pattern;

import static org.animotron.statement.operator.Utils.*;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class BinaryMapExpression extends AbstractExpression {

    private String path;
    private String uriContext;

    public BinaryMapExpression(String path, String uriContext) {
        this(new FastGraphBuilder(), path, uriContext);
    }

    public BinaryMapExpression(GraphBuilder builder, String path, String uriContext) {
        super(builder);
        this.path = path;
        this.uriContext = uriContext;
    }

    @Override
    public void build() throws IOException, AnimoException {
        builder.start(THE._);
            builder.start(AN._);
                builder._(REF._, FILE);
            builder.end();
            builder.start(AN._);
                builder._(REF._, URI);
                builder._(uriContext + path);
            builder.end();
            Iterator<String> it = new StringArrayIterator(path.split(Pattern.quote(File.separator)));
            while (it.hasNext()) {
                String i = it.next();
                for (String s : new StringArrayIterator(i.split(Pattern.quote(".")))) {
                    builder.start(AN._);
                    builder._(REF._, s);
                    builder.end();
                }
                if (!it.hasNext()) {
                    Iterator<String> jt = new StringArrayIterator(i.split(Pattern.quote(".")));
                    if (jt.hasNext()) {
                        builder.start(AN._);
                            builder._(REF._, NAME);
                            builder._(jt.next());
                        builder.end();
                    }
                    if (jt.hasNext()) {
                        builder.start(AN._);
                            builder._(REF._, EXTENSION);
                            do {
                                builder._(jt.next());
                            } while (jt.hasNext());
                        builder.end();
                    }
                }
            }
        builder.end();
    }

}
