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


    public CommonExpression(File file) throws IOException {
        this(file, file.getName());
    }
    public CommonExpression(GraphBuilder builder, File file) throws IOException {
        this(builder, file, file.getName());
    }


    public CommonExpression(File file, String path) throws IOException {
        this(new FileInputStream(file), path);
    }
    public CommonExpression(GraphBuilder builder, File file, String path) throws IOException {
        this(builder, new FileInputStream(file), path);
    }

    public CommonExpression(InputStream stream) {
        e = new AnimoExpression(stream);
    }
    public CommonExpression(GraphBuilder builder, InputStream stream) {
        e = new AnimoExpression(builder, stream);
    }

    public CommonExpression(InputStream stream, String path) {
        e = isAnimo(path) ? new AnimoExpression(stream) : new BinaryExpression(stream, path);
    }
    public CommonExpression(GraphBuilder builder, InputStream stream, String path) {
        e = isAnimo(path) ? new AnimoExpression(builder, stream) : new BinaryExpression(builder, stream, path);
    }


	private static boolean isAnimo(String path) {
		return path.endsWith(".animo");
	}


    @Override
    protected Relationship relationship() {
        return e.relationship();
    }

}
