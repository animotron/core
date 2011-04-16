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
package org.animotron.exist.interpreter;

import org.animotron.exist.index.AnimoIndex;
import org.animotron.exist.index.AnimoIndexWorker;
import org.exist.Database;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Controller {

	private Database db;
	
	public Controller(Database db) {
		this.db = db;
	}
	
	public AnimoIndexWorker getIndexWorker() {
        return (AnimoIndexWorker) db.getActiveBroker().getIndexController().getWorkerByIndexId(AnimoIndex.ID);
	}

	public NodeSet getUse(NodeSet set) {
		//XXX: code
//      let $query := concat("unordered($CONTEXT/", local:name-sequence($is, "descendant-or-self::use"), ")")
//      let $use   := local:eval-query($query, (), $context, ())
		return set;
	}
	
	//TODO: write tests
	public NodeSet preferedUse(NodeSet is) {
		if (is.isEmpty())
			return NodeSet.EMPTY_SET;
		
		NodeSet result = new NewArrayNodeSet();
		
		NodeSet use = getUse(is);
		for (NodeProxy node : use) {
			NodeSet useIS = getIndexWorker().resolveDownIsLogic(node);

			if (useIS.isEmpty())
				continue;
			
			NodeSet res = use.intersection(useIS);
			
			if (res.isEmpty())
				result.add(node);
			else
				result.addAll(res);
		}
		
		return result;
	}
}
