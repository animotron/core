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

	@Before
	public void beforeTests() throws Exception {
		Synchro synchro = Synchro._;
		startDB("data-test");
		initChannels();
	}

	public void initChannels() throws Exception {
		channel2 = new JChannel();//It's a cluster

		channel2.setReceiver(new ReceiverAdapter() {
			@Override
			public void receive(Message msg) {
				System.out.println("channel2 received msg from " + msg.getSrc() + ": " + msg.getObject());
			}
			@Override
			public void viewAccepted(View new_view) {
			}
		});

	    channel2.connect("AnimoCluster");
	}

	@Test
	public void test_00() throws Exception {
		String s =
			"PREVIOUSHASH:7eb83afdaa9f0bfeb627d9ea9100c8dc34fb3ad5a00a22eb6d07e7f081b90c9a|" +
			"HASH:3e7120f442b9c671fdddf5e5aa2f02e916b7666d076a938b51728bb73404b9df|INSTANCE:" +
			"def goods01 " +
				"(goods) "+
				"(color \"red\") " +
				"(name \"goods-name04\") " +
			".";

		channel2.send(new Message(null, null, s));

		Thread.sleep(10000);
	    channel2.close();

	}
}