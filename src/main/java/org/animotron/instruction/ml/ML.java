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
package org.animotron.instruction.ml;

import org.animotron.annotation.Namespace;
import org.animotron.instruction.AbstractContainer;

/**
 * Instructions container 'ml' - makeup language.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
@Namespace(prefix = "ml", uri = "animo/ml")
public class ML extends AbstractContainer {
	
	private static class SingletonHolder { 
		public static final ML INSTANCE = new ML();
	}
	
	public static ML getInstance() {
		return SingletonHolder.INSTANCE;
	}
	
	private ML() {}
}
