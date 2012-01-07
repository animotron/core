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
import org.animotron.expression.BinaryExpression.StringArrayIterator;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.THE;
import org.animotron.statement.value.STREAM;
import org.animotron.utils.MessageDigester;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.util.Iterator;
import java.util.regex.Pattern;

import static org.animotron.graph.Nodes.*;
import static org.animotron.utils.MessageDigester.byteArrayToHex;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class BinaryMapExpression extends AbstractExpression {

    private File file;
    private String path;
    private String uriContext;

    public BinaryMapExpression(File file, String path, String uriContext) {
        this(new FastGraphBuilder(), file, path, uriContext);
    }

    public BinaryMapExpression(GraphBuilder builder, File file, String path, String uriContext) {
        super(builder);
        this.file = file;
        this.path = path;
        this.uriContext = uriContext;
    }

    @Override
    public void build() throws IOException, AnimoException {
        byte buf[] = new byte[1024 * 4];
        int len;
        InputStream stream = new FileInputStream(file);
        MessageDigest md = MessageDigester.md();
        while((len=stream.read(buf))>0) {
            md.update(buf,0,len);
        }
        builder.start(THE._);
            builder.start(AN._);
                builder._(REF._, FILE);
            builder.end();
            builder.start(AN._);
                builder._(REF._, URI);
                builder._(uriContext + path + "?" + byteArrayToHex(md.digest()));
            builder.end();
            builder._(STREAM._, file.getPath());
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
