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

import org.animotron.utils.MessageDigester;

import java.io.*;
import java.security.MessageDigest;

import static org.animotron.graph.AnimoGraph.binStorage;
import static org.animotron.graph.AnimoGraph.tmpStorage;
import static org.animotron.utils.MessageDigester.*;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public abstract class BinaryExpression extends AbstractBinaryExpression {

    private InputStream stream;
    private boolean closeStream = true;
    private String hash;

    public BinaryExpression(InputStream stream, boolean closeStream) {
        super();
        this.stream = stream;
        this.closeStream = closeStream;
    }

    @Override
    public void build() throws Throwable {
        String uuid = uuid().toString();
        File tmp = new File(tmpStorage(), uuid);
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
        hash = byteArrayToHex(md.digest()) + longToHex(size);
        File dir = getFolder(hash);
        File bin = getFile(dir, hash);
        if (bin.exists()) {
            tmp.delete();
        } else {
            dir.mkdirs();
            if (!tmp.renameTo(bin)) {
                tmp.delete();
                throw new IOException("transaction can not be finished");
            }
        }
        super.build();
    }

    @Override
    public String id() {
        return hash();
    }

    protected String hash() {
        return hash;
    }

    private File getFolder(String hash) {
        return new File(new File(binStorage(), hash.substring(0, 2)), hash.substring(0, 4));
    }

    private File getFile(File folder, String hash){
        return new File(folder,  hash);
    }

}
