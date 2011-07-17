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

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.junit.Ignore;
import org.junit.Test;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class MimeTest extends ATest {
	
	private static final String MIME = 
		"<the:text-plain " + ATest.ANIMO_NSs + ">" +
		"   <is:mime-type/>" +
		"   <is:text/>" +
		"   <have:type>text/plain</have:type>" +
		"   <have:name>Plain text</have:name>" +
		"   <have:extension>txt</have:extension>" +
		"</the:text-plain>";
	
	private static final String STATEMENT = 
		"<any:mime-type " + ATest.ANIMO_NSs + ">" +
		"  	<eq:extension>txt</eq:extension>" +
		"</any:mime-type>";
	
	@Test
	@Ignore
	public void storeAndSerializeResult() throws XMLStreamException {
        System.out.println("Test processing flow interator ...");
        
        CommonBuilder.build(MIME);
        CommonBuilder.build(STATEMENT);

        
        System.out.println("loaded ...");
            
        System.out.println("done.");
	}
}
