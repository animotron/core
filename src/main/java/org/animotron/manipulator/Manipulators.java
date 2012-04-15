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

import static org.animotron.graph.Properties.HASH;
import static org.animotron.graph.RelationshipTypes.REV;
import static org.animotron.utils.MessageDigester.byteArrayToHex;
import javolution.util.FastTable;

import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.statement.operator.THE;
import org.animotron.synchro.Synchro;
import org.neo4j.graphdb.Direction;
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

		public Catcher() {}
		
        public void preparative(Node node) {
            preparative.add(node);
        }

        //TODO refacor catcher
		public void clear() {
			modified = null;
            preparative.clear();
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

		public void push() throws Throwable {
			preparative();
            modified();
			destructive();
		}

        private void preparative() throws IOException {
			for (Node n : preparative) {
            	for (Relationship r : n.getRelationships(THE._)) {
            		Node nn = THE._.getActualRevision(r);
            		Relationship prev = nn.getSingleRelationship(REV, Direction.INCOMING);
            		String previousHash = "";
            		if(prev != null)
                		previousHash = byteArrayToHex((byte[]) HASH.get(prev));
            		String hash = byteArrayToHex((byte[]) HASH.get(r));

            		Synchro._.sendDataToChannel("PREVIOUSHASH:" + previousHash + "|HASH:" + hash + "|INSTANCE:" + CachedSerializer.ANIMO.serialize(r));
            	}
				Preparator._.execute(null, n);
			}
        }

        private void modified() throws Throwable {
            if (modified != null) {
                //CachedSerializer.drop(modified);
                //DependenciesTracking._.execute(null, modified);
            }
        }

        private void destructive() throws IOException {
			for (Node n : destructive) {
				GC._.execute(null, n);
			}
		}

    }
	
	public static Catcher getCatcher() {
		return _.new Catcher();
	}

}
