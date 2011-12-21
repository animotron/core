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
public class ZipBridge {

	static final int BUFFER = 2048;

	public static void load(String path) throws IOException {
		load(new File(path));
	}

	public static void load(File file) throws IOException {
		if (!file.exists()) {
			return;
		}
		FileInputStream fis = new FileInputStream(file);
		ZipInputStream zis = new ZipInputStream(new BufferedInputStream(fis));

		ZipEntry entry;
		while ((entry = zis.getNextEntry()) != null) {
			System.out.println("Extracting: " + entry);
			__(new CommonExpression(zis, entry.getName(), false));
		}
		zis.close();
	}

	private static void loadDir(String root, File path) throws IOException {
		for (File file : path.listFiles()) {
			load(root, file);
		}
	}

	private static void loadFile(String root, File file) throws IOException {
		String path = file.getPath().substring(root.length());
		__(new CommonExpression(file, path));
	}

	private static void load(String root, File path) throws IOException {
		if (path.isDirectory()) {
			loadDir(root, path);
		} else {
			loadFile(root, path);
		}
	}

}
