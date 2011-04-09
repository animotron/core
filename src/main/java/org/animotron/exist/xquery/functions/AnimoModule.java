package org.animotron.exist.xquery.functions;

import java.util.List;
import java.util.Map;

import org.animotron.Namespaces;
import org.exist.xquery.AbstractInternalModule;
import org.exist.xquery.FunctionDef;

public class AnimoModule extends AbstractInternalModule {

    public static final String NAMESPACE_URI = Namespaces.ANIMO.namespace();
    
    public static final String PREFIX = Namespaces.ANIMO.prefix();
    public final static String INCLUSION_DATE = "2011-04-09";
    public final static String RELEASED_IN_VERSION = "eXist-1.5";
    
    public static final FunctionDef[] functions = {
        new FunctionDef(ResolveIsLogic.signature[0], ResolveIsLogic.class),
        new FunctionDef(ResolveIsLogic.signature[1], ResolveIsLogic.class)
    };
    
	public AnimoModule(FunctionDef[] functions, Map<String, List<? extends Object>> parameters) {
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
