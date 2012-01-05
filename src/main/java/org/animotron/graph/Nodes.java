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

import org.animotron.statement.operator.THE;
import org.neo4j.graphdb.Node;


/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Nodes {

    public static Node EXTENSION;
    public static Node MIME_TYPE;
    public static Node TYPE;
    public static Node FILE;
    public static Node NAME;
    public static Node URI;

    static {
        new GraphOperation<Void>() {
            @Override
            public Void execute() throws Exception {
                EXTENSION = THE._("extension");
                MIME_TYPE = THE._("mime-type");
                TYPE = THE._("type");
                FILE = THE._("file");
                NAME = THE._("name");
                URI = THE._("uri");
                return null;
            }
        };
    }


}
