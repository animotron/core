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
package org.animotron.utils;

import com.eaio.uuid.UUID;
import org.animotron.statement.Statement;
import org.apache.log4j.Logger;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import static org.animotron.graph.Properties.NAME;

public class MessageDigester {

    private static String[] hex = 
    	{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"};

    private static final Logger LOG = Logger.getLogger(MessageDigester.class);

	private static final String CACHE_ALGORITHM = "SHA-256";

    public static MessageDigest md() {
        try {
            return MessageDigest.getInstance(CACHE_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            //can't be, but throw runtime error
            throw new RuntimeException(e);
        }
    }

    public static String calculate(String message, String algorithm) throws IllegalArgumentException {

        // Can throw a  NoSuchAlgorithmException
        MessageDigest  md = null;
        try {
            md = MessageDigest.getInstance(algorithm);

        } catch (NoSuchAlgorithmException e) {
            String error = "'"+ algorithm + "' is not a supported MessageDigest algorithm.";
            LOG.error(error, e);
            throw new IllegalArgumentException(error);
        }

        // Calculate hash
        md.update( message.getBytes() );
        byte[] digestData = md.digest();

        // Write digest as string
        String digest = null;
        digest = byteArrayToHex( digestData );

        return  digest;
    }

    private static void byteToHex( StringBuilder buf, byte b ) {
        int n = b;
        if ( n < 0 ) {
            n = 256 + n;
        }
        int d1 = n / 16;
        int d2 = n % 16;
        buf.append( hex[d1] );
        buf.append( hex[d2] );
    }

    public static String byteArrayToHex( byte[] b ) {
        StringBuilder buf = new StringBuilder( b.length * 2 );
        for ( int i = 0; i < b.length; i++ ) {
            byteToHex( buf, b[i] );
        }
        return buf.toString();
    }

    public static byte[] longToByteArray( long b ) {
        byte[] target = new byte[8];
        for ( int i = 0; i < 8; i++ ) {
            target[7-i] = (byte) (b >>> (i * 8));
        }
        return target;
    }


    public static String longToHex( long b ) {
        return byteArrayToHex(longToByteArray(b));
    }

    public static final MessageDigest cloneMD(MessageDigest md) {
        try {
            return (MessageDigest) md.clone();
        } catch (CloneNotSupportedException e) {
            //can't be, but throw runtime error
            throw new RuntimeException(e);
        }
    }

    public static void updateMD(MessageDigest md, Statement statement) {
        md.update(statement.name().getBytes());
    }

    @SuppressWarnings("unchecked")
	public static void updateMD(MessageDigest md, Object reference) {
        if (reference instanceof Iterable) {
            updateMD(md, (Iterable<Object[]>) reference);
        } else if (reference instanceof Object[][]) {
            updateMD(md, (Object[][]) reference);
        } else if (reference instanceof Object[]) {
            updateMD(md, (Object[]) reference);
        } else if (reference instanceof Node) {
            updateMD(md, NAME.get((Node) reference));
        } else if (reference instanceof Relationship) {
            updateMD(md, NAME.get(((Relationship) reference).getEndNode()));
        } else if (reference instanceof String ||
                        reference instanceof Number ||
                            reference instanceof Boolean) {
            md.update(reference.toString().getBytes());
        }
    }

    private static void updateMD(MessageDigest md, Iterable<Object[]> reference) {
        for (Object[] o: reference) {
            updateMD(md, o);
        }
    }

    private static void updateMD(MessageDigest md, Object[][] reference) {
        for (Object[] o: reference) {
            updateMD(md, o);
        }
    }

    private static void updateMD(MessageDigest md, Object[] reference) {
        updateMD(md, (Statement) reference[0], reference[1]);
    }

    private static void updateMD(MessageDigest md, Statement statement, Object reference) {
        MessageDigest tmp = MessageDigester.md();
        updateMD(tmp, reference);
        updateMD(tmp, statement);
        md.update(tmp.digest());
    }

    public static String uuid() {
        return new UUID().toString();
    }

    public static UUID uuid(String uuid) {
        return new UUID(uuid);
    }

}
/*
 *  eXist Open Source Native XML Database
 *  Copyright (C) 2010-2011 The eXist Project
 *  http://exist-db.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  $Id: MessageDigester.java 13770 2011-02-12 18:43:33Z shabanovd $
 */