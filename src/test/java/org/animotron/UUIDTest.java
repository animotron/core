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
package org.animotron;

import com.eaio.uuid.UUID;
import com.eaio.uuid.UUIDGen;
import junit.framework.Assert;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class UUIDTest extends ATest {

    @Test
	public void test_00() throws Throwable {

    	long st = System.nanoTime();
    	Thread.sleep(1);
    	UUID uuid = new UUID(System.nanoTime(), UUIDGen.getClockSeqAndNode());
    	Thread.sleep(1);
    	long et = System.nanoTime();
    	
    	Assert.assertNotNull(uuid);

    	long time = uuid.getTime();
    	System.out.println(st);
    	System.out.println(time);
    	System.out.println(et);
        Assert.assertTrue("time less then start time ["+st+" -> "+time+"]", st <= time);
        Assert.assertTrue("time bigger then end time ["+st+" -> "+time+"]", et >= time);
        
        String str = uuid.toString();
    	uuid = new UUID(str);

    	time = uuid.getTime();
    	System.out.println(time);
        Assert.assertTrue("time less then start time ["+st+" -> "+time+"]", st <= time);
        Assert.assertTrue("time bigger then end time ["+st+" -> "+time+"]", et >= time);
    }
}