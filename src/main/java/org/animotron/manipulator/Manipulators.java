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
package org.animotron.manipulator;

import javolution.util.FastTable;
import org.animotron.graph.serializer.CachedSerializer;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Manipulators {
	
	private static Manipulators _ = new Manipulators();
	
	private Manipulators() {}
	
	//TODO: Implement manipulators/listeners/broadcasters loader
	
	public class Catcher {
		
		FastTable<Node> preparative = new FastTable<Node>();
		FastTable<Node> destructive = new FastTable<Node>();
        Relationship modified = null;
        Node creative = null;

		public Catcher() {}
		
        public void preparative(Node node) {
            preparative.add(node);
        }

		public void preparative(Relationship r) {
			preparative(r.getEndNode());
		}

        public void modified(Relationship r) {
            modified = r;
        }

        public void destructive(Node node) {
            destructive.add(node);
        }

        public void destructive(Relationship r) {
            destructive(r.getEndNode());
            r.delete();
        }

		public void push() throws IOException {
			preparative();
            modified();
			destructive();
		}

        private void preparative() throws IOException {
            if (creative != null)
                Preparator._.execute(null, creative);
        }

        private void modified() throws IOException {
            if (modified != null)
                CachedSerializer.drop(modified);
        }

        private void destructive() throws IOException {
			//for (Node n : destructive) {
				//XXX: GC._.execute(n);
			//}
		}

    }
	
	public static Catcher getCatcher() {
		return _.new Catcher();
	}

}
