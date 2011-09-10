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
import org.neo4j.graphdb.Relationship;

import java.io.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class CommonBuilder {
	
	public static Relationship build(String data) throws AnimoException, IOException {
        return storeAnimo(new AnimoBuilder(data));
	}
	
    public static Relationship build(File file) throws IOException, AnimoException {
        return build(file, file.getName());
    }
	
    public static Relationship build(File file, String path) throws IOException, AnimoException {
        return build(new FileInputStream(file), path);
    }

	public static Relationship build(InputStream stream) throws AnimoException, IOException {
		return storeAnimo(new AnimoBuilder(stream));
	}
	
	public static Relationship build(InputStream stream, String path) throws IOException, AnimoException {
        return  isAnimo(path) ? storeAnimo(new AnimoBuilder(stream)) : storeBinary(stream, path);

    }
	
	private static boolean isAnimo(String path) {
		return path.endsWith(".animo");
	}
	
	private static Relationship storeAnimo(AnimoBuilder builder) throws AnimoException, IOException {
        builder.build();
		return builder.getRelationship();
	}

	private static Relationship storeBinary(InputStream stream, String path) throws IOException, AnimoException {
		return new BinaryBuilder(stream, path);
	}

}
