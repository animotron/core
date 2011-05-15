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
package org.animotron.exist.index;

import static org.junit.Assert.*;

import java.util.LinkedHashMap;
import java.util.Map;

import org.animotron.exist.AbstractTest;
import org.exist.EXistException;
import org.exist.storage.DBBroker;
import org.junit.Test;
import org.neo4j.graphdb.Node;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class EvaluationTreeTests extends AbstractTest {
	
	private static final String THE_A = 
		"<the:A "+ANIMO_NSs+"/>";
	
	private static final String THE_B = 
		"<the:B "+ANIMO_NSs+">" +
		"	<is:A/>" +
		"	<have:B>" +
		"		<some/>" +
		"		<another>" +
		"			<one-more/>" +
		"		</another>" +
		"	</have:B>" +
		"</the:B>";

	@Test
	public void processingFlowInterator() {
        System.out.println("Test processing flow interator ...");
        
        Map<String, String> nameDataMap = new LinkedHashMap<String, String>();
        nameDataMap.put("A.xml", THE_A);
        nameDataMap.put("B.xml", THE_B);
        
        configureAndStore(COLLECTION_CONFIG, nameDataMap);
        
        
        DBBroker broker = null;
        try {
            broker = pool.get(pool.getSecurityManager().getSystemSubject());
            assertNotNull(broker);

            Node node = AnimoGraph.getTHE("B");
            
            assertNotNull(node);
            
            String[] must = new String[] {"the:B", "have:B", "some", "another"};
            int i = 0;
            
            ProcessingFlowIterator it = new ProcessingFlowIterator(node);
            while (it.hasNext()) {
            	System.out.println(it.next().getProperty("name"));
            	//System.out.println(AnimoGraph.getNodeProxy(it.next()).getNode().getNodeName());
            	//assertEquals("on "+i+" step", must[i], AnimoGraph.getNodeProxy(it.next()).getNode().getNodeName());
            	i++;
            }
            
        } catch (EXistException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} finally {
        	pool.release(broker);
        }
        System.out.println("done.");
	}
}
