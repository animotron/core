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
package org.animotron.ssd;

import org.animotron.ATest;
import org.junit.Test;

import java.io.IOException;

public class SimpleTest extends ATest {

	@Test
	public void test_00() throws IOException {
        __("def ki:u-d34f43f0-8621-3fab-9710-804c8ceeae06?v=2 " +
        		"(mm:businessActivity) " +
        		"(CreationDate \"2013-03-30T23:00:17.317+01:00\") " +
        		"(Author usr:abarone/diviana.net) " +
        		"(label " +
        		"(lang:it_IT \"Visura ISEE\") " +
        		"(lang:en-US \"Family income survey\")) " +
        		"(Input ki:u-9cb59ef5-b2ec-3232-a68a-54b3a127db53?v=2) " +
        		"(Output ki:u-07edfef6-fef7-3e55-89da-c457e64f2238?v=2).");

        __("def ki:u-9cb59ef5-b2ec-3232-a68a-54b3a127db53?v=2 " +
        		"(mm:parallelSplit) " +
        		"(CreationDate \"2013-03-30T23:00:17.317+01:00\") " +
        		"(Author usr:abarone/diviana.net) " +
        		"(label " +
        		"lang:it_IT \"r\") " +
        		"(Output " +
        		"(ki:u-6990f479-cc83-3960-8726-c46d372455b4?v=2) " +
        		"(ki:u-d34f43f0-8621-3fab-9710-804c8ceeae06?v=2)).");
        
        __("def ki:u-15c69254-0625-446f-a2bb-033e434c12f7?v=2 " +
        		"(mm:performs) " +
        		"(CreationDate \"2013-03-30T23:00:17.317+01:00\") " +
        		"(Author usr:abarone/diviana.net) " +
        		"(Subject ki:u-640d03c0-9f53-3f48-91ed-8a961e292baa?v=2) " +
        		"(Object ki:u-6990f479-cc83-3960-8726-c46d372455b4?v=2).");
        
        __("def ki:u-640d03c0-9f53-3f48-91ed-8a961e292baa?v=2 " +
        		"(^ki:u-7a08dca5-fd45-32ea-81b1-439156e64c30?v=2) " +
        		"(mm:role) " +
        		"(CreationDate \"2013-03-30T23:00:17.317+01:00\") " +
        		"(Author usr:abarone/diviana.net) " +
        		"(label " +
        		"(lang:it_IT \"Impiegato\") " +
        		"(lang:en_US \"Employee\")).");
        
        __("def usr:abarone/diviana.net " +
        		"(mm:authenticatedUser) " +
        		"(CreationDate \"2012-11-20T10:53:32.688+01:00\") " +
        		"(Email \"abarone@diviana.net\") " +
        		"(FirstName \"Annalisa\") " +
        		"(LastName \"Barone\") " +
        		"(label " +
        		"(lang:it_IT (get FirstName) (get LastName)) " +
        		"(lang:ru_RU (get LastName) (get FirstName))).");
        
//        assertAnimoResult("what-is a", "x.");
	}

}