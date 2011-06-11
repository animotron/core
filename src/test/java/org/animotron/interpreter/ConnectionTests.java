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
package org.animotron.interpreter;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTests extends ATest {
	
	private static final String THE_MIME_TYPE = 
		"<the:mime-type "+ANIMO_NSs+">" +
		"	<have:extension/>" +
		"</the:mime-type>";
	
	private static final String THE_FILE = 
		"<the:file "+ANIMO_NSs+">" +
		"	<have:name>file</have:name>"+
		"	<have:path/>"+

		"	<ic:extension>"+
		"		<string:after-last>"+
		"			<ml:text>.</ml:text>"+
		"			<get:path/>"+
		"		</string:after-last>"+
		"	</ic:extension>"+
		"	<ic:mime-type>"+
		"		<any:mime-type>"+
		"			<have:extension>"+
		"				<get:extension/>"+
		"			</have:extension>"+
		"		</any:mime-type>"+
		"	</ic:mime-type>"+
		"</the:file>";

	private static final String THE_FILEA = 
		"<the:fileA "+ANIMO_NSs+">" +
		"	<is:file/>" +
		
		"	<have:path>/home/test.txt</have:path>" +
		
		"</the:fileA>";
	
	private static final String THE_TEXT_PLAIN = 
		"<the:text-plain "+ANIMO_NSs+">" +
		"	<is:mime-type/>" +
		
		"	<have:type>text/plain</have:type>" +

	    "	<have:extension>txt</have:extension>" +
		"	<have:extension>text</have:extension>" +
		
		"</the:text-plain>";

	private static final String THE_A = 
		"<the:A "+ANIMO_NSs+">" +
		"	<get:type>" +
		"		<get:mime-type>" +
		"			<an:fileA/>" +
		"		</get:mime-type>" +
		"	</get:type>" +
		"</the:A>";

	
	@Test
	public void mimeType_usecase() throws IOException, XMLStreamException {
        System.out.println("Mime type use case ...");
        
        if (firstRun) {
	        Map<String, String> nameDataMap = new LinkedHashMap<String, String>();
	        nameDataMap.put("mime-type.xml", THE_MIME_TYPE);
	        nameDataMap.put("file.xml", THE_FILE);
	        nameDataMap.put("fileA.xml", THE_FILEA);
	        nameDataMap.put("text-plain.xml", THE_TEXT_PLAIN);
	        nameDataMap.put("A.xml", THE_A);
	        
	        store(nameDataMap);
        }
        
        //System.out.println("");
        assertEquals("A", "<the:A><have:type>text/plain</have:type></the:A>");

        //System.out.println("done.");
	}
	
}
