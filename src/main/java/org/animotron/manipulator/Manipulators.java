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
package org.animotron.manipulator;

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Manipulators {
	
	private static Manipulators _ = new Manipulators();
	
	private Manipulators() {}
	
	//TODO: Implement manipulators/listeners/broadcasters loader
	
	public class Catcher {
		
//        List<Node> creative = new FastList<Node>();
//        List<Node> destructive = new FastList<Node>();
        List<Node> creative = new LinkedList<Node>();
        List<Node> destructive = new LinkedList<Node>();

		public Catcher() {}
		
        public void creative(Node node) {
            creative.add(node);
        }

		public void creative(Relationship r) {
			creative(r.getEndNode());
		}
		
        public void destructive(Node node) {
            destructive.add(node);
        }
		
        public void destructive(Relationship r) {
            destructive(r.getEndNode());
            r.delete();
        }

		public void push() throws IOException {
			creative();
			destructive();
		}
		
		private void creative() throws IOException {
			for (Node n : creative) {
				Preparator._.execute(n);
			}
		}
		
		private void destructive() throws IOException {
			for (Node n : destructive) {
				GC._.execute(new PFlow(GC._), n);
			}
		}
		
	}
	
	public static Catcher getCatcher() {
		return _.new Catcher();
	}

}
