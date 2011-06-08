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
package org.animotron.graph;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.util.UUID;

import org.animotron.MessageDigester;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class BinanyBuilder extends GraphBuilder {
	
	private final static File STORAGE = new File(AnimoGraph.STORAGE, "binany");
	
	static {
		STORAGE.mkdir();
	}
	
	private InputStream stream;
	private String path;
	
	public BinanyBuilder(InputStream stream, String path) {
		this.stream = stream;
		this.path = path;
	}
	
	public Relationship build() {
		
		String txID = UUID.randomUUID().toString();
		
		try {
			File tmp = new File(STORAGE, txID);
			tmp.createNewFile();
			OutputStream out = new FileOutputStream(tmp);
			
			byte buf[]=new byte[1024 * 4];
			int len;
			MessageDigest md = md();
			
			while((len=stream.read(buf))>0) {
				out.write(buf,0,len);
				md.update(buf,0,len);
			}
			
			String hash = MessageDigester.byteArrayToHex(md.digest());
			System.out.println(hash);
			
			out.close();
			stream.close();
			
			File bin = new File(STORAGE, hash);
			
			if (bin.exists()) {
				tmp.delete();
			} else {
				tmp.renameTo(bin);
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}

		//TODO: Store binary and build graph for one
		return getRelationship();
	}

}
