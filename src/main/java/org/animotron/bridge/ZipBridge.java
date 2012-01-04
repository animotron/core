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

import org.animotron.expression.CommonExpression;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import static org.animotron.expression.Expression.__;

/**
 * Repository loader
 * 
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class ZipBridge extends AbstractFSBridge{

    public static final ZipBridge _ = new ZipBridge();

    private ZipBridge(){}

	static final int BUFFER = 2048;

    @Override
	public void load(File file, String uriContext) throws IOException {
		if (!file.exists()) {
			return;
		}
		FileInputStream fis = new FileInputStream(file);
		ZipInputStream zis = new ZipInputStream(new BufferedInputStream(fis));

		ZipEntry entry;
		while ((entry = zis.getNextEntry()) != null) {
			System.out.println("Extracting: " + entry);
			__(new CommonExpression(zis, entry.getName(), uriContext, false));
		}
		zis.close();
	}

    @Override
    protected void load(File file, String path, String uriContext) throws IOException {
    }

}
