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

import org.animotron.graph.builder.GraphBuilder;
import org.neo4j.graphdb.Relationship;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class CommonExpression extends Expression {

    private AbstractExpression e;

    public CommonExpression(String data) throws Exception {
        e = new AnimoExpression(data);
    }
    public CommonExpression(GraphBuilder builder, String data) {
        e = new AnimoExpression(builder, data);
    }


    public CommonExpression(File file, String uriContext) throws IOException {
        this(file, file.getName(), uriContext);
    }
    public CommonExpression(GraphBuilder builder, File file, String uriContext) throws IOException {
        this(builder, file, file.getName(), uriContext);
    }


    public CommonExpression(File file, String path, String uriContext) throws IOException {
        this(new FileInputStream(file), path, uriContext, true);
    }
    public CommonExpression(GraphBuilder builder, File file, String path, String uriContext) throws IOException {
        this(builder, new FileInputStream(file), path, uriContext, true);
    }

    public CommonExpression(InputStream stream) {
        e = new AnimoExpression(stream);
    }
    public CommonExpression(GraphBuilder builder, InputStream stream) {
        e = new AnimoExpression(builder, stream);
    }

    public CommonExpression(InputStream stream, String path, String uriContext) {
    	this(stream, path, uriContext, true);
    }
    public CommonExpression(InputStream stream, String path, String uriContext, boolean closeStream) {
        e = isAnimo(path) ? new AnimoExpression(stream) : new BinaryExpression(stream, path, uriContext, closeStream);
    }

    public CommonExpression(GraphBuilder builder, InputStream stream, String path, String uriContext) {
    	this(builder, stream, path, uriContext, true);
    }
    public CommonExpression(GraphBuilder builder, InputStream stream, String path, String uriContext, boolean closeStream) {
        e = isAnimo(path) ? new AnimoExpression(builder, stream) : new BinaryExpression(builder, stream, path, uriContext, closeStream);
    }

	private static boolean isAnimo(String path) {
		return path.endsWith(".animo");
	}

    @Override
    protected Relationship relationship() {
        return e.relationship();
    }
}