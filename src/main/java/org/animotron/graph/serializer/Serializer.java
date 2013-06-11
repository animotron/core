/*
 *  Copyright (C) 2011-2013 The Animo Project
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
package org.animotron.graph.serializer;

import javolution.util.FastSet;
import org.animotron.cache.Cache;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.handler.AnimoGraphHandler;
import org.animotron.graph.handler.AnimoPrettyGraphHandler;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.handler.TextGraphHandler;
import org.animotron.graph.traverser.AnimoResultOneStepTraverser;
import org.animotron.graph.traverser.AnimoResultTraverser;
import org.animotron.graph.traverser.AnimoTraverser;
import org.animotron.graph.traverser.ResultTraverser;
import org.animotron.manipulator.QCAVector;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.lang.reflect.Field;

import static org.animotron.graph.AnimoGraph.execute;
import static org.animotron.graph.Properties.CACHE;
import static org.animotron.graph.Properties.RUUID;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Serializer {
	
    public static Serializer STRING = new Serializer(ResultTraverser._, ".txt") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        return new TextGraphHandler(out);
		}
        @Override
        protected GraphHandler handler(Writer out) throws IOException {
            return new TextGraphHandler(out);
        }
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        return new TextGraphHandler(out);
		}
    };
	
    public static Serializer PRETTY_ANIMO_RESULT = new Serializer(AnimoResultTraverser._, "-res-pretty.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
	        return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        return new AnimoPrettyGraphHandler(out);
		}
	};
	
	public static Serializer PRETTY_ANIMO = new Serializer(AnimoTraverser._, "-src-pretty.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoPrettyGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoPrettyGraphHandler(out);
		}
	};

	public static Serializer ANIMO_RESULT = new Serializer(AnimoResultTraverser._, "-res.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};
	
	public static Serializer ANIMO = new Serializer(AnimoTraverser._, "-src.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};

	public static Serializer ANIMO_RESULT_ONE_STEP = new Serializer(new AnimoResultOneStepTraverser(), "-1step-res.animo") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(Writer out) {
			return new AnimoGraphHandler(out);
		}
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};

    private AnimoTraverser traverser;

    private Serializer(AnimoTraverser traverser) {
        this.traverser = traverser;
    }

    private Serializer(AnimoTraverser traverser, String ext){
        this(traverser);
        this.ext = ext;
    }

    protected abstract GraphHandler handler(OutputStream out) throws IOException;

    protected abstract GraphHandler handler(Writer out) throws IOException;

    protected abstract GraphHandler handler(StringBuilder out);

    public final void serialize(Relationship r, OutputStream out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final void serialize(QCAVector v, OutputStream out) throws IOException {
        traverser.traverse(handler(out), v);
    }

    public final void serialize(Relationship r, StringBuilder out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final void serialize(QCAVector v, StringBuilder out) throws IOException {
        traverser.traverse(handler(out), v);
    }

    public final void serialize(Relationship r, Writer out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final void serialize(QCAVector v, Writer out) throws IOException {
        traverser.traverse(handler(out), v);
    }

    public String serialize(Relationship r) throws IOException {
        StringBuilder out = new StringBuilder(256);
        traverser.traverse(handler(out), r);
        return out.toString();
    }

    public String serialize(QCAVector v) throws IOException {
        StringBuilder out = new StringBuilder(256);
        traverser.traverse(handler(out), v);
        return out.toString();
    }
	private String ext;

    private static String key(String uuid, String ext) throws IOException {
        StringBuilder s = new StringBuilder(2);
        s.append(uuid);
        s.append(ext);
        return s.toString();
    }

    private String uuid(Relationship r) {
        return RUUID.has(r) ? (String) RUUID.get(r) : MessageDigester.uuid().toString();
    }

    public final void serialize(Relationship r, OutputStream out, Cache cache) throws Throwable {
        serialize(r, out, cache, uuid(r));
    }

    public final void serialize(Relationship r, OutputStream out, Cache cache, String uuid) throws Throwable {
        String key = key(uuid, ext);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, uuid);
            } catch (Throwable t) {
                cache.drop(key);
                throw t;
            }
            os.close();
        }
    }

    public final void serialize(Relationship r, StringBuilder out, Cache cache) throws Throwable {
        serialize(r, out, cache, uuid(r));
    }

    public final void serialize(Relationship r, StringBuilder out, Cache cache, String uuid) throws Throwable {
        String key = key(uuid, ext);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, uuid);
            } catch (Throwable t) {
                cache.drop(key);
                throw t;
            }
            os.close();
        }
    }

    public final String serialize(Relationship r, Cache cache) throws Throwable {
        return serialize(r, cache, uuid(r));
    }

    public final String serialize(Relationship r, Cache cache, String uuid) throws Throwable {
        String key = key(uuid, ext);
        if (cache.available(key)) {
            return cache.get(key);
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
                cache(r, cache, uuid);
            } catch (Throwable t) {
                cache.drop(key);
                throw t;
            }
            os.close();
            return out.toString();
        }
    }
    
    private void cache(final Relationship r, final Cache cache, final String uuid) throws Throwable {
        final FastSet<String> entities = FastSet.newInstance();
        entities.add(entity(cache));
        if (CACHE.has(r))
            for (String e : (String[]) CACHE.get(r))
                entities.add(e);
        execute(new GraphOperation<Void>() {
            @Override
            public Void execute() throws Throwable {
                RUUID.set(r, uuid);
                CACHE.set(r, (String[]) entities.toArray());
                return null;
            }
        });
    }
    
    private String entity(Cache cache) {
        return ext + "@" + cache.getClass().getName();
    }

    private static void drop(String uuid, String entity) throws IOException {
        String[] token = entity.split("@");
        try {
            Class<?> clazz = Class.forName(token[1]);
            Field field = clazz.getField("_");
            Cache cache = (Cache) field.get(clazz);
            StringBuilder s = new StringBuilder(2);
            cache.drop(key(uuid, token[0]));
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (NoSuchFieldException e) {
            e.printStackTrace();
        }
    }

    public static void drop(Relationship r) throws Throwable {
        if (CACHE.has(r)) {
            String uuid = (String) RUUID.get(r);
            try {
                for (String i : (String[]) CACHE.get(r)) {
                    drop(uuid, i);
                }
            } catch (IOException e) {
                throw e;
            } finally {
                CACHE.remove(r);
                RUUID.remove(r);
            }
        }
    }

}