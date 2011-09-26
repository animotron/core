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
package org.animotron.bridge;

import org.animotron.exception.AnimoException;
import org.animotron.expression.CommonExpression;

import java.io.File;
import java.io.IOException;

/**
 * Repository loader
 * 
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class FSBridge {
	
    public static void load (String path) throws IOException, AnimoException {
        load(new File(path));
    }

    public static void load (File path) throws IOException, AnimoException {
        if (path.isDirectory()) {
            loadDir(path.getPath(), path);
        } else {
            loadFile(path.getParent(), path);
        }
    }

    private static void loadDir (String root, File path) throws IOException, AnimoException {
        for (File file : path.listFiles()) {
            load(root, file);
        }
    }

    private static void loadFile (String root, File file) throws IOException, AnimoException {
        String path = file.getPath().substring(root.length());
        CommonExpression.build(file, path);
    }

	private static void load (String root, File path) throws IOException, AnimoException {
		
		if (path.isDirectory()) {
            loadDir(root, path);
		} else {
            loadFile(root, path);
		}
		
	}
	
}
