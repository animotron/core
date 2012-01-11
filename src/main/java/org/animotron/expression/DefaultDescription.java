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
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.REF;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.regex.Pattern;

import static org.animotron.graph.Nodes.EXTENSION;
import static org.animotron.graph.Nodes.NAME;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class DefaultDescription {

    public static void create(GraphBuilder builder, String path) throws AnimoException, IOException {
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
    }

    public static class StringArrayIterator implements Iterable<String>, Iterator<String> {

        private String[] a;
        private String c = null;
        int i = 0;

        public StringArrayIterator(String[] a) {
            this.a = a;
            next();
        }

        @Override
        public Iterator<String> iterator() {
            return this;
        }

        @Override
        public boolean hasNext() {
            return c != null;
        }

        @Override
        public String next() {
            String next = c;
            c = step();
            return next;
        }

        private String step() {
            if (i < a.length) {
                String s = a[i]; i++;
                if (s == null || s.isEmpty()) {
                   return step();
                } else {
                    return s;
                }
            } else {
                return null;
            }
        }

        @Override
        public void remove() {
        }

    }

}
