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

import org.animotron.Keywords;
import org.animotron.Namespaces;
import org.exist.dom.ElementImpl;
import org.exist.memtree.MemTreeBuilder;
import org.exist.xquery.XPathException;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ProcessReference extends Process {
	
	ProcessReference(Controller controller) {
		super(controller);
		ns = Namespaces.AN.namespace();
	}

	public void process (ElementImpl input, MemTreeBuilder builder) throws XPathException{
		
		String name = input.getLocalName();
		
		if (Keywords.AN_EMPTY.keyword().equals(name)) {
			// process an:empty
			// return nothing
			return;

		} else if (Keywords.AN_CONTENT.keyword().equals(name)) {
			// process an:content
			// process children
			getController().processChild(input, builder);

		} else if (Keywords.AN_SELF.equals(name, ns)) {
			// process an:self
			// return root
			builder.addReferenceNode(input.getDocument().getFirstChildProxy());
			
		} else {
			// process reference an:*
			return;
			
		}
		
	}
	
}
