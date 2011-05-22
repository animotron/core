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

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import org.apache.log4j.Logger;
import org.neo4j.graphdb.Node;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class ReverseAnimoGraphBuilder {
	
	private static final Logger LOG = Logger.getLogger(ReverseAnimoGraphBuilder.class);
	
	private static final String HASH_ALGOTHIM = "SHA-256";
	
	private int level = 0;
	
	private Stack<MessageDigest> hashStack = new Stack<MessageDigest>();
	private Stack<List<Node>> childrenStack = new Stack<List<Node>>();

	public void startElement(String ns, String name) {
		level++;
		
		MessageDigest md;
		try {
			md = MessageDigest.getInstance(HASH_ALGOTHIM);
		} catch (NoSuchAlgorithmException e) {
			//can't be, but throw runtime error
			throw new RuntimeException(e);
		}
		//hash-function depend on namespace & name
		md.update(ns.getBytes());
		md.update(name.getBytes());
		hashStack.push(md);
		
		//create children list
		childrenStack.push(new LinkedList<Node>());
	}

	public void endElement(String ns, String name) {
		MessageDigest md = hashStack.pop();
		
		//current element hash-function value
		byte[] elementDigest = md.digest();
		
		if (level != 1) {
			//update parent's
			hashStack.peek().update(elementDigest);
		}
		
		//current node's children
		List<Node> children = childrenStack.pop();
		
		Node currentNode = findOrCreate(elementDigest, ns, name, children);
		
		//add this node as child
		childrenStack.peek().add(currentNode);
		
		level--;
	}

	public void attribute(String ns, String name, String value) {
		MessageDigest md = hashStack.peek();
		//hash-function depend on namespace, name & value
		md.update(ns.getBytes());
		md.update(name.getBytes());
		md.update(value.getBytes());
	}

	public void characters(String text) {
		MessageDigest md = hashStack.peek();
		//hash-function depend on characters
		md.update(text.getBytes());
	}

	private Node findOrCreate(byte[] elementDigest, String ns, String name, List<Node> children) {
		// TODO Auto-generated method stub
		return null;
	}
}
