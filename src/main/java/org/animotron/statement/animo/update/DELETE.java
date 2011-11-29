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
package org.animotron.statement.animo.update;

import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class DELETE extends AbstractUpdate {

    public static final DELETE _ = new DELETE();

	private DELETE() {super("delete");}

    @Override
    protected void execute(QCAVector destination, Relationship pattern, IndexHits<Relationship> target) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    protected void execute(PFlow pf, IndexHits<Relationship> params) throws IOException {
        if (params.size() > 2) {
            for (Relationship r : params) {
                for (QCAVector i : Evaluator._.execute(pf, r)) {
                    execute(i, null, null);
                }
            }
        } else if (params.hasNext()) {
            for (QCAVector i : Evaluator._.execute(pf, params.next())) {
                execute(i, params.next(), null);
            }
        }
    }

}