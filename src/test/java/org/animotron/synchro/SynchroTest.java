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

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.graph.AnimoGraph.startDB;

import org.animotron.ATest;
import org.jgroups.JChannel;
import org.jgroups.Message;
import org.jgroups.ReceiverAdapter;
import org.jgroups.View;
import org.junit.Before;
import org.junit.Test;

/**
 * @author <a href="mailto:amir.akhmedov@gmail.com">Amir Akhmedov</a>
 *
 */

/**
 * It's important to set -Djava.net.preferIPv4Stack=true JVM argument
 * to run unit test 
 * 
 */
public class SynchroTest extends ATest{

	public static JChannel channel2;

	private boolean messageSentByCluster = false;

	@Before
	public void beforeTests() throws Exception {
		startDB("data-test");
		initChannels();
	}

	public void initChannels() throws Exception {
		channel2 = new JChannel();//It's a cluster

		channel2.setReceiver(new ReceiverAdapter() {
			@Override
			public void receive(Message msg) {
				System.out.println("channel2 received msg from " + msg.getSrc() + ": " + msg.getObject());

				if(channel2.getAddress() != msg.getSrc() && messageSentByCluster == false) {
					messageSentByCluster = true;
					try {
						channel2.send(new Message(null, null, "the goods01 (goods) (name \"goods-name02\")."));
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
			@Override
			public void viewAccepted(View new_view) {
			}
		});

	    channel2.connect("AnimoCluster");
	}

	@Test
	public void test_00() throws Exception {
		__(
			"the goods01" +
				"(goods) "+
				"(name \"goods-name01\") " +
			"."
		);

		Thread.sleep(5000);
	    channel2.close();
	}
}