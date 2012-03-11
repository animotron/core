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

import org.animotron.statement.operator.THE;
import org.jgroups.ChannelException;
import org.jgroups.JChannel;
import org.jgroups.Message;
import org.jgroups.ReceiverAdapter;
import org.jgroups.View;
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.Properties.HASH;

/**
 * @author <a href="mailto:amir.akhmedov@gmail.com">Amir Akhmedov</a>
 *
 */
public class Synchro {

	public static JChannel channel1;

	public Synchro() {
		System.setProperty("jgroups.bind_addr", "127.0.0.1");
		System.setProperty("jgroups.tcpping.initial_hosts", "127.0.0.1[7800]");

		try {
			channel1 = new JChannel();

			channel1.setReceiver(new ReceiverAdapter() {
				@Override
				public void receive(Message msg) {
					System.out.println("channel1 received msg from " + msg.getSrc() + ": " + msg.getObject());

					checkInstance((String)msg.getObject());
//					if(channel1.getAddress() != msg.getSrc()) {
//						__((String)msg.getObject());
//					}
				}
				@Override
				public void viewAccepted(View new_view) {
				}
			});
	
		    channel1.connect("AnimoCluster");
	
//		    channel1.close();//XXX: When close it?
		} catch (ChannelException e) {
			e.printStackTrace();
		}
	}

	public void sendDataToChannel(String message) {
		if(channel1 != null)
			try {
				channel1.send(new Message(null, null, message));
			} catch (Exception e) {
				e.printStackTrace();
			}
	}

	private void checkInstance(String message) {
		int fromIndex = message.indexOf(" ") + 1;
		String instanceName = message.substring(fromIndex, message.indexOf(" ", fromIndex));

		Relationship instance = THE._.get(instanceName);
		Object hash = HASH.get(instance);
	}
}