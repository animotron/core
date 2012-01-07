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
package org.animotron.bridge;

import java.io.File;
import java.io.IOException;

/**
 * Repository loader
 * 
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractFSBridge {

    public void load (String path, String uriContext) throws IOException {
        load(new File(path), uriContext);
    }

    public void load (File path, String uriContext) throws IOException {
        if (!path.exists()) {
            return;
        }
        if (path.isDirectory()) {
            loadDir(path.getPath(), path, uriContext);
        } else {
            loadFile(path.getParent(), path, uriContext);
        }
    }

    private void loadDir (String root, File path, String uriContext) throws IOException {
        for (File file : path.listFiles()) {
            load(root, file, uriContext);
        }
    }

    private void loadFile (String root, File file, String uriContext) throws IOException {
        load(file, file.getPath().substring(root.length()), uriContext);
    }

    abstract protected void load(File file, String path, String uriContext) throws IOException;

	private void load (String root, File path, String uriContext) throws IOException {
		if (path.isDirectory()) {
            loadDir(root, path, uriContext);
		} else {
            loadFile(root, path, uriContext);
		}
	}
	
}
