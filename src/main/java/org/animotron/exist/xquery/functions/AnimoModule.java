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
package org.animotron.exist.xquery.functions;

import java.util.List;
import java.util.Map;

import org.animotron.Namespaces;
import org.exist.xquery.AbstractInternalModule;
import org.exist.xquery.FunctionDef;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoModule extends AbstractInternalModule {

    public static final String NAMESPACE_URI = Namespaces.ANIMO.namespace();
    
    public static final String PREFIX = Namespaces.ANIMO.prefix();
    public final static String INCLUSION_DATE = "2011-04-09";
    public final static String RELEASED_IN_VERSION = "eXist-1.5";
    
    public static final FunctionDef[] functions = {
        new FunctionDef(Process.signature[0], Process.class),
        new FunctionDef(Process.signature[1], Process.class),
        new FunctionDef(ResolveIsLogic.signature[0], ResolveIsLogic.class),
        new FunctionDef(ResolveIsLogic.signature[1], ResolveIsLogic.class)
    };
    
	public AnimoModule(Map<String, List<? extends Object>> parameters) {
		super(functions, parameters);
	}

	public String getDefaultPrefix() {
		return PREFIX;
	}

	public String getDescription() {
		return "Animotron XQuery module";
	}

	public String getNamespaceURI() {
		return NAMESPACE_URI;
	}

	public String getReleaseVersion() {
		return "eXist 1.5";
	}

}
