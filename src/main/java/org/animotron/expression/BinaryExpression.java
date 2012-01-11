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

import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.utils.MessageDigester;

import java.io.*;
import java.security.MessageDigest;
import java.util.UUID;

import static org.animotron.graph.AnimoGraph.getStorage;
import static org.animotron.utils.MessageDigester.byteArrayToHex;
import static org.animotron.utils.MessageDigester.longToHex;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public abstract class BinaryExpression extends AbstractBinaryExpression {

	private final static String BIN_STORAGE = "binary";
	private final static String TMP_STORAGE = "tmp";

    private InputStream stream;
    private boolean closeStream = true;
    private String id;
    private File bin;

    private File binStorage(){
        return new File(getStorage(), BIN_STORAGE);
    }

    private File tmpStorage(){
        return new File(getStorage(), TMP_STORAGE);
    }

    public BinaryExpression(InputStream stream, boolean closeStream) {
        super(new FastGraphBuilder());
        this.stream = stream;
        this.closeStream = closeStream;
    }



    @Override
    public void build() throws Exception {
        id = UUID.randomUUID().toString();
        File tmp = new File(tmpStorage(), id);
        tmp.createNewFile();
        OutputStream out = new FileOutputStream(tmp);
        byte buf[] = new byte[1024 * 4];
        int len;
        long size = 0;
        MessageDigest md = MessageDigester.md();
        while((len=stream.read(buf))>0) {
            out.write(buf,0,len);
            md.update(buf,0,len);
            size += len;
        }
        out.close();
        if (closeStream) stream.close();
        
        String hash = byteArrayToHex(md.digest()) + longToHex(size);
        File dir = getFolder(hash);
        bin = getFile(dir, hash);
        if (bin.exists()) {
            tmp.delete();
            System.out.println("File \"" + bin.getPath() + "\" already stored");
        } else {
            dir.mkdirs();
            if (!tmp.renameTo(bin)) {
                tmp.delete();
                throw new IOException("transaction can not be finished");
            }
            System.out.println("Store the file \"" + bin.getPath() + "\"");
        }
        buildExpression();
    }

    @Override
    protected String stream() {
        return bin.getPath();
    }

    @Override
    protected String id() {
        return id;
    }

    private File getFolder(String hash) {
        return new File(new File(binStorage(), hash.substring(0, 2)), hash.substring(0, 4));
    }

    private File getFile(File folder, String hash){
        return new File(folder,  hash);
    }

}
