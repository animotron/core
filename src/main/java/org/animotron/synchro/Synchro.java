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
package org.animotron.synchro;

import org.animotron.expression.AnimoExpression;
import org.animotron.statement.operator.ASHIFT;
import org.animotron.statement.operator.DEF;
import org.jgroups.*;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

import static org.animotron.graph.Properties.HASH;
import static org.animotron.graph.RelationshipTypes.SHIFT;
import static org.animotron.expression.Expression.__;
import static org.animotron.utils.MessageDigester.byteArrayToHex;

/**
 * @author <a href="mailto:amir.akhmedov@gmail.com">Amir Akhmedov</a>
 *
 */
public class Synchro {

	public static JChannel channel1;

	public static final Synchro _ = new Synchro();

	public Synchro() {
		System.setProperty("jgroups.bind_addr", "127.0.0.1");
		System.setProperty("jgroups.tcpping.initial_hosts", "127.0.0.1[7800]");

		try {
			channel1 = new JChannel();

			channel1.setReceiver(new ReceiverAdapter() {
				@Override
				public void receive(Message msg) {
					System.out.println("channel1 received msg from " + msg.getSrc() + ": " + msg.getObject());

					if(channel1.getAddress() != msg.getSrc()) {
						checkInstance((String)msg.getObject());
					}
				}
				@Override
				public void viewAccepted(View new_view) {
				}
			});
	
		    channel1.connect("AnimoCluster");
	
//		    channel1.close();//XXX: When do close it?
		} catch (ChannelException e) {
			e.printStackTrace();
		}
	}

	public void sendDataToChannel(String message) throws IOException {
        try {
		if(channel1 != null)
            channel1.send(new Message(null, null, message));
        } catch (ChannelNotConnectedException e) {
            throw new IOException(e);
        } catch (ChannelClosedException e) {
            throw new IOException(e);
        }
    }

	private void checkInstance(String message) {
		String synchPreviousHash = message.substring(0, message.indexOf("|"));
		synchPreviousHash = synchPreviousHash.substring(synchPreviousHash.indexOf(":") + 1);

		message = message.substring(message.indexOf("|")+1);
		String synchHash = message.substring(0, message.indexOf("|"));
		synchHash = synchHash.substring(synchHash.indexOf(":") + 1);
		
		String synchContent = message.substring(message.indexOf("|") + 1);
		synchContent = synchContent.substring(synchContent.indexOf(":") + 1);

		int fromIndex = synchContent.indexOf(" ") + 1;
		String synchName = synchContent.substring(fromIndex, synchContent.indexOf(" ", fromIndex));

		Relationship instance = DEF._.get(synchName);

		String instanceHash = "";
		String instancePreviousHash = "";
		if(instance != null) {
			instanceHash = byteArrayToHex((byte[]) HASH.get(instance));

			Node nn = ASHIFT._.actualNode(instance);
			Relationship prev = nn.getSingleRelationship(SHIFT, Direction.INCOMING);
			if(prev != null) {
				instancePreviousHash = byteArrayToHex((byte[]) HASH.get(prev));
			}
		}

//		System.out.println("synchHash = " + synchHash + " synchPreviousHash = " + synchPreviousHash +
//				" instanceHash = " + instanceHash + " instancePreviousHash = " + instancePreviousHash);

		if(synchPreviousHash.equals(instancePreviousHash) || synchPreviousHash == "" || instancePreviousHash == "") {
			if (instance == null || synchHash != instanceHash) {
	            __(new AnimoExpression(synchContent));
			}
		}
	}
}