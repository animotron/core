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

import org.animotron.expression.BinaryMapExpression;

import java.io.File;
import java.io.IOException;

import static org.animotron.expression.Expression.__;

/**
 * Map FS to animo
 * 
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class FSMap extends AbstractFSBridge {

    public static final FSMap _ = new FSMap();

    private FSMap(){}

    @Override
    protected void load (File file, String path, String uriContext) throws IOException {
        __(new BinaryMapExpression(path, uriContext));
    }

}
