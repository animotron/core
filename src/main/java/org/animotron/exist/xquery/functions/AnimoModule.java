package org.animotron.exist.xquery.functions;

import java.util.List;
import java.util.Map;

import org.animotron.AnimoNamespaces;
import org.exist.xquery.AbstractInternalModule;
import org.exist.xquery.FunctionDef;

public class AnimoModule extends AbstractInternalModule {

	public AnimoModule(FunctionDef[] functions, Map<String, List<? extends Object>> parameters) {
		super(functions, parameters);
	}

	public String getDefaultPrefix() {
		return AnimoNamespaces.ANIMO.prefix();
	}

	public String getDescription() {
		return "Animotron XQuery module";
	}

	public String getNamespaceURI() {
		return AnimoNamespaces.ANIMO.namespace();
	}

	public String getReleaseVersion() {
		return "eXist 1.5";
	}

}
