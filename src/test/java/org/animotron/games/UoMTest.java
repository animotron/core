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
package org.animotron.games;

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
public class UoMTest extends ATest {

	private static final String THE_UoM = 
		"<the:UoM "+ANIMO_NSs+">" +
		"</the:UoM>";

	private static final String THE_METER = 
		"<the:meter "+ANIMO_NSs+">" +
		"	<is:UoM/>" +
		"</the:meter>";

	private static final String THE_MEASURE = 
		"<the:measure "+ANIMO_NSs+">" +
		"	<have:quantity/>" +
		"	<have:UoM/>" +
		"</the:measure>";

	private static final String THE_M_10m = 
		"<the:m-10m "+ANIMO_NSs+">" +
		"	<is:measure/>" +
		"	<have:quantity><Q:N10/></have:quantity>" +
		"	<have:UoM><an:meter/></have:UoM>" +
		"</the:m-10m>";

	private static final String THE_M_5m = 
		"<the:m-5m "+ANIMO_NSs+">" +
		"	<have:quantity><Q:N5/></have:quantity>" +
		"	<have:UoM><an:meter/></have:UoM>" +
		"</the:m-5m>";

	private static final String THE_A = 
		"<the:A "+ANIMO_NSs+">" +
		"	<math:sum>" +
		"		<an:m-10m/>" +
		"		<an:m-5m/>" +
		"	</math:sum>" +
		"</the:A>";

	private static final String THE_B = 
		"<the:B "+ANIMO_NSs+">" +
		"	<math:sum>" +
		"		<an:m-5m/>" +
		"		<an:m-10m/>" +
		"	</math:sum>" +
		"</the:B>";

	@Test
	public void operations() throws XMLStreamException, IOException {
		
		if (true) {
	        Map<String, String> data = new LinkedHashMap<String, String>();

	        data.put("UoM.xml", THE_UoM);
	        data.put("meter.xml", THE_METER);
	        data.put("measure.xml", THE_MEASURE);
	        data.put("m-10m.xml", THE_M_10m);
	        data.put("m-5m.xml", THE_M_5m);

	        data.put("A.xml", THE_A);
	        data.put("B.xml", THE_B);

	        store(data);
		}

        //NOTE: R must be same not only for start&end node, but between A & B

		//System.out.println("op:plus an:m-10m an:m-5m");
//        assertAnimo(A, "<the:A><the:{*=R}><is:measure/><have:quantity><Q:N15/></have:quantity><have:UoM><an:meter/></have:UoM></the:{*=R}></the:A>");

        //System.out.println("op:plus an:m-5m an:m-10m");
//        assertAnimo(B, "<the:B><the:{*=R}><is:measure/><have:quantity><Q:N15/></have:quantity><have:UoM><an:meter/></have:UoM></the:{*=R}></the:B>");

	}
}
