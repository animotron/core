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

import org.animotron.Properties;
import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.IS;
import org.animotron.utils.MessageDigester;

import java.io.*;
import java.security.MessageDigest;
import java.util.UUID;
import java.util.regex.Pattern;

import static org.animotron.graph.AnimoGraph.getStorage;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class BinaryExpression extends Expression {
	
	private final static File BIN_STORAGE = new File(getStorage(), "binany");
	private final static File TMP_STORAGE = new File(getStorage(), "tmp");

    private InputStream stream;
    private String path;

    static {
		BIN_STORAGE.mkdirs();
		TMP_STORAGE.mkdirs();
	}

    public BinaryExpression(InputStream stream, String path) throws Exception {
        super(new FastGraphBuilder());
        this.stream = stream;
        this.path = path;
        builder.build(this);
    }

    @Override
    public void build() throws IOException, AnimoException {
        String txID = UUID.randomUUID().toString();
        File tmp = new File(TMP_STORAGE, txID);
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
        File dir = getFolder(hash);
        File bin = getFile(dir, hash);
        if (bin.exists()) {
            tmp.delete();
            System.out.println("File \"" + bin.getPath() + "\" already stored");
        } else {
            dir.mkdirs();
            if (!tmp.renameTo(bin)) {
                tmp.delete();
                throw new IOException("transaction can not be finished");

            } else {
                builder.startGraph();
                    builder.start(THE._);
                        builder.start(IS._, "file");
                        builder.end();
                        String[] parts = path.split(Pattern.quote(File.separator));
                        //JExpression prev = null;
                        for (String part : parts) {
                            if (!part.isEmpty()) {
                                builder.start(IS._, part);
                                builder.end();
                            }
                        }
                    builder.end();
                builder.endGraph();
                Properties.BIN.set(getEndNode(), hash);
            }
            System.out.println("Store the file \"" + bin.getPath() + "\"");
        }
    }

    private static File getFolder(String hash) {
        return new File(new File(BIN_STORAGE, hash.substring(0, 2)), hash.substring(0, 4));
    }

    private static File getFile(File folder, String hash){
        return new File(folder,  hash);
    }

    public static File getFile(String hash){
        return new File(getFolder(hash),  hash);
    }

}
