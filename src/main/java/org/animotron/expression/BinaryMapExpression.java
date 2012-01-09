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

import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.utils.MessageDigester;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.MessageDigest;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public abstract class BinaryMapExpression extends AbstractBinaryExpression {

    private File file;
    MessageDigest md = MessageDigester.md();

    public BinaryMapExpression(File file) {
        super(new FastGraphBuilder());
        this.file = file;
    }

    @Override
    public void build() throws Exception {
        byte buf[] = new byte[1024 * 4];
        int len;
        InputStream stream = new FileInputStream(file);
        while((len=stream.read(buf))>0) {
            md.update(buf,0,len);
        }
        stream.close();
        buildExpression();
    }

    @Override
    protected String stream() {
        return file.getPath();
    }

    @Override
    protected String id() {
        return null;
    }

}
