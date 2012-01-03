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
package org.animotron.graph.serializer;

import com.ctc.wstx.api.WriterConfig;
import com.ctc.wstx.stax.WstxOutputFactory;
import org.animotron.cache.Cache;
import org.animotron.graph.handler.*;
import org.animotron.graph.traverser.*;
import org.animotron.manipulator.QCAVector;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.io.OutputStream;

import static org.animotron.utils.MessageDigester.byteArrayToHex;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class CachedSerializer extends AbstractSerializer {
	
    public static CachedSerializer STRING = new CachedSerializer(ResultTraverser._, ".txt") {
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        return new TextGraphHandler(out);
		}
		
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        return new TextGraphHandler(out);
		}
	};
	
	public static CachedSerializer XML = new CachedSerializer(MLResultTraverser._, ".xml") {
		
	    public final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();

		{
	        WriterConfig conf = OUTPUT_FACTORY.getConfig();
	        conf.doSupportNamespaces(true);
	        conf.enableAutomaticNamespaces(false);
		}
		
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        throw new UnsupportedOperationException();
		}
		
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        try {
	            return new StAXGraphHandler(OUTPUT_FACTORY.createXMLStreamWriter(out));
	        } catch (XMLStreamException e) {
	            throw new IOException(e);
	        }
		}
	};

    public static CachedSerializer HTML = new CachedSerializer(MLResultTraverser._, ".html") {
        @Override
        protected GraphHandler handler(StringBuilder out) {
            return new HtmlGraphHandler(out);
        }

        @Override
        protected GraphHandler handler(OutputStream out) throws IOException {
            return new HtmlGraphHandler(out);
        }
    };

    public static CachedSerializer HTML_PART = new CachedSerializer(MLResultTraverser._, ".html.part") {
        @Override
        protected GraphHandler handler(StringBuilder out) {
            return new HtmlPartGraphHandler(out);
        }

        @Override
        protected GraphHandler handler(OutputStream out) throws IOException {
            return new HtmlPartGraphHandler(out);
        }
    };

    public static CachedSerializer PRETTY_ANIMO_RESULT = new CachedSerializer(AnimoResultTraverser._, "-res-pretty.animo") {
		
		@Override
		protected GraphHandler handler(StringBuilder out) {
	        return new AnimoPrettyGraphHandler(out);
		}
		
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
	        return new AnimoPrettyGraphHandler(out);
		}
	};
	
	public static CachedSerializer PRETTY_ANIMO = new CachedSerializer(AnimoTraverser._, "-src-pretty.animo") {
		
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoPrettyGraphHandler(out);
		}
		
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoPrettyGraphHandler(out);
		}
	};

	public static CachedSerializer ANIMO_RESULT = new CachedSerializer(AnimoResultTraverser._, "-res.animo") {
		
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};
	
	public static CachedSerializer ANIMO = new CachedSerializer(AnimoTraverser._, "-src.animo") {
		
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};

	public static CachedSerializer ANIMO_RESULT_ONE_STEP = new CachedSerializer(new AnimoResultOneStepTraverser(), "-1step-res.animo") {
		
		@Override
		protected GraphHandler handler(StringBuilder out) {
			return new AnimoGraphHandler(out);
		}
		
		@Override
		protected GraphHandler handler(OutputStream out) throws IOException {
			return new AnimoGraphHandler(out);
		}
	};

	private String ext;

    protected CachedSerializer(AnimoTraverser traverser, String ext){
        super(traverser);
        this.ext = ext;
	}

    private String key (Relationship r) throws IOException {
        StringBuilder s = new StringBuilder(2);
        s.append(byteArrayToHex(DigestSerializer._.serialize(r)));
        s.append(ext);
        return s.toString();
    }

    private String key (QCAVector v) throws IOException {
        return key(v.getClosest());
    }

    public final void serialize(Relationship r, OutputStream out, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
        }
    }

    public final void serialize(QCAVector v, OutputStream out, Cache cache) throws IOException {
        String key = key(v);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(v, os);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
        }
    }

    public final void serialize(Relationship r, StringBuilder out, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
        }
    }

    public final void serialize(QCAVector v, StringBuilder out, Cache cache) throws IOException {
        String key = key(v);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            try {
                serialize(v, os);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
        }
    }

    public final String serialize(Relationship r, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            return cache.get(key);
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream os = cache.stream(key, out);
            try {
                serialize(r, os);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
            return out.toString();
        }
    }

    public final String serialize(QCAVector v, Cache cache) throws IOException {
        String key = key(v);
        if (cache.available(key)) {
            return cache.get(key);
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream os = cache.stream(key, out);
            try {
                serialize(v, os);
            } catch (IOException e) {
                cache.drop(key);
                throw e;
            }
            os.close();
            return out.toString();
        }
    }
}