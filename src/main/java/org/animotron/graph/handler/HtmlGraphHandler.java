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
package org.animotron.graph.handler;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class HtmlGraphHandler extends HtmlPartGraphHandler {

    public HtmlGraphHandler(GraphHandler gh) {
        super(gh);
    }

    public HtmlGraphHandler(OutputStream out) {
        super(out);
    }
    public HtmlGraphHandler(StringBuilder out) {
        super(out);
    }

    @Override
    public void startGraph() throws IOException {
        write("<!DOCTYPE html>");
	}

}