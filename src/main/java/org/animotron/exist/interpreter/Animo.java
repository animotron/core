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
package org.animotron.exist.interpreter;

import java.io.IOException;
import java.util.Properties;

import org.exist.interpreter.Compiled;
import org.exist.interpreter.Compiler;
import org.exist.interpreter.Context;
import org.exist.security.xacml.AccessContext;
import org.exist.source.Source;
import org.exist.storage.XQueryPool;
import org.exist.xquery.XPathException;
import org.exist.xquery.value.Sequence;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Animo implements Compiler {

	public Context newContext(AccessContext accessCtx) {
		// TODO Auto-generated method stub
		return null;
	}

	public XQueryPool getXQueryPool() {
		// TODO Auto-generated method stub
		return null;
	}

	public Compiled compile(Context context, String expression)
			throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

	public Compiled compile(Context context, Source source)
			throws XPathException, IOException {
		// TODO Auto-generated method stub
		return null;
	}

	public Compiled compile(Context context, Source source, boolean xpointer)
			throws XPathException, IOException {
		// TODO Auto-generated method stub
		return null;
	}

	public Sequence execute(Compiled expression, Sequence contextSequence)
			throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

	public Sequence execute(Compiled expression, Sequence contextSequence,
			Properties outputProperties) throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

	public Sequence execute(Compiled expression, Sequence contextSequence,
			boolean resetContext) throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

	public Sequence execute(Compiled expression, Sequence contextSequence,
			Properties outputProperties, boolean resetContext)
			throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

	public Sequence execute(String expression, Sequence contextSequence,
			AccessContext accessCtx) throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

}
