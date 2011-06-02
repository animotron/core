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

import java.util.StringTokenizer;

import org.animotron.Quanta;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.instruction.InstructionContainer;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.instruction.ml.TEXT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class XMLGraphBuilder extends AbstractGraphBuilder {
	
	public void startElement(String ns, String name) {
		
		Statement statement;
		Quanta container = Statements.namespace(ns);
		
		if (container instanceof InstructionContainer) {
			statement = ((InstructionContainer) container).getInstruction(name);
		} else {
			statement = (Statement) container;
		}
		
		if (statement == null) 
			statement = ELEMENT.getInstance();
		
		start(statement, ns, name, null);
		
	}
	
	public void attribute(String ns, String name, String value) {
		start(ATTRIBUTE.getInstance(), ns, name, value);
		end();
	}

	public void text (String text) {
		
		StringBuilder buf = new StringBuilder();
		if (text.length() > 0) {
			StringTokenizer tok = new StringTokenizer(text);
			while (tok.hasMoreTokens()) {
                buf.append(tok.nextToken());
				if (tok.hasMoreTokens()) buf.append(' ');
			}
		}
		
		if (buf.length() > 0) {
			start(TEXT.getInstance(), null, null, buf.toString());
			end();
		}
			
	}
		
	public void comment(String text) {
		start(COMMENT.getInstance(), null, null, text);
		end();
	}
	
	public void cdata (String text) {
		start(CDATA.getInstance(), null, null, text);
		end();
	}
	
	public void endElement(String ns, String name) {
		end();
	}
	
}
