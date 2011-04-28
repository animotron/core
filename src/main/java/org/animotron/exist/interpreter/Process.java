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

import org.exist.dom.ElementAtExist;
import org.exist.memtree.MemTreeBuilder;
import org.exist.storage.DBBroker;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.Item;
import org.exist.xquery.value.NodeValue;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.Type;
import org.w3c.dom.Node;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Process {

	final public Controller controller;
	
	Process(Controller controller){
		this.controller = controller;
	}
	
	abstract public Sequence eval() throws XPathException;
	
	public void process (MemTreeBuilder builder) throws XPathException {
		process(eval(), builder);
	}
	
	public XQueryContext getXQueryContext(){
		return controller.getXQueryContext();
	}
	
	public DBBroker getBroker(){
		return getXQueryContext().getBroker();
	}
	
	public Sequence getContext(){
		return controller.getContext();
	}
	
	public Sequence getLocalContext(){
		return controller.getLocalContext();
	}
	
	public ElementAtExist getCurrentFlow(){
		return controller.getCurrentFlow();
	}
	
	public Node getCurrentStep(){
		return controller.getCurrentStep();
	}
	
	public Sequence getContextStack(){
		return controller.getContextStack();
	}
	
	public Sequence getFlowStack(){
		return controller.getFlowStack();
	}
	
	public void process(Sequence input, MemTreeBuilder builder) throws XPathException {
		if (input == null)
			return;
		
		SequenceIterator i = input.iterate();
		while (i.hasNext()){
			Item item = i.nextItem();
			if (Type.getSuperType(item.getType()) == Type.NODE) {
				controller.process((NodeValue) item, builder);
			} else {
				builder.characters(item.getStringValue());
			}
		}
	}
}
