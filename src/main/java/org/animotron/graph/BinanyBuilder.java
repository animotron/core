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

import static org.animotron.Expression.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.util.UUID;

import org.animotron.Expression;
import org.animotron.MessageDigester;
import org.animotron.operator.THE;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class BinanyBuilder {
	
	private final static File STORAGE = new File(AnimoGraph.STORAGE, "binany");
	private final static File TMP = new File(STORAGE, "tmp");
	
	static {
		STORAGE.mkdir();
		TMP.mkdirs();
	}
	
	private InputStream stream;
	private String path;
	
	public BinanyBuilder(InputStream stream, String path) {
		this.stream = stream;
		this.path = path;
	}
	
	public Relationship build() throws IOException {
		
		String txID = UUID.randomUUID().toString();
		
		File tmp = new File(TMP, txID);
		tmp.createNewFile();
		OutputStream out = new FileOutputStream(tmp);
		
		byte buf[] = new byte[1024 * 4];
		int len;
		MessageDigest md = MessageDigester.md();
		
		while((len=stream.read(buf))>0) {
			out.write(buf,0,len);
			md.update(buf,0,len);
		}
		
		out.close();
		stream.close();
		
		String hash = MessageDigester.byteArrayToHex(md.digest());
		
		File l1  = new File(STORAGE, hash.substring(0, 2));  
		File l2  = new File(l1, hash.substring(0, 4));  
		File bin = new File(l2,  hash);
		
		if (bin.exists()) {
			
			tmp.delete();
			System.out.println("File \"" + bin.getPath() + "\" already stored");
			
			return (THE.getInstance().relationship(hash));
			
		} else {
			
			Relationship r = null;
			l2.mkdirs();
			
			if (!tmp.renameTo(bin)) {
				tmp.delete();
				throw new IOException("transaction can not be finished");
				
			} else {
				Expression e = new Expression(
						s(THE.getInstance(), hash,
							s(IS.getInstance(), "file"),
							s(HAVE.getInstance(), "path", text(path))
						)
					);
				r = e.build();
				
				if (!e.successful()) {
					tmp.delete();
				}
				
			}
			
			System.out.println("Store the file \"" + bin.getPath() + "\"");
			
			return r;
			
		}
			
	}

}
