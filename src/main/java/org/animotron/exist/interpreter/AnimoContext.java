package org.animotron.exist.interpreter;

import java.util.HashMap;
import java.util.Map;

import net.sf.saxon.type.Type;

import org.animotron.Namespaces;
import org.exist.dom.ElementImpl;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class AnimoContext {
	
	private NodeSet set;
	
	private Map <String, NodeSet> instances = new HashMap <String, NodeSet> ();
	private Map <String, NodeSet> properties = new HashMap <String, NodeSet> ();
	
	public AnimoContext(NodeSet set){
		this.set = set;
		scan(set);
	}
	
	public NodeSet getNodes(){
		return set;
	}
	
	private void scan(ElementImpl node, Map <String, NodeSet> map){
		String name = node.getLocalName();
		NodeSet set = map.get(name);
		if (set == null) {
			set = new NewArrayNodeSet();
			map.put(name, set);
		}
		NodeProxy proxy = new NodeProxy(node);
		set.add(proxy);
	}
	
	private void scan(ElementImpl node){
		String ns = node.getNamespaceURI();
		if (Namespaces.THE.equals(ns)){
			scan(node, instances);
		} else if (Namespaces.HAVE.equals(ns) || Namespaces.IC.equals(ns)){
			scan(node, properties);
		}
		scan(node.getChildNodes());
	}
	
	private void scan(NodeSet set){
		for (NodeProxy i : set){
			if (i.getType() == Type.ELEMENT){
				ElementImpl node = (ElementImpl) i.getNode();
				scan(node);
			}
		}
	}
	
	private void scan(NodeList set){
		for (int i = 0; i < set.getLength(); i++){
			Node node = set.item(i);
			if (node.getNodeType() == Type.ELEMENT){
				scan((ElementImpl)node);
			}
		}
	}
	
	public NodeSet resolveReference(String name){
		return instances.get(name); 
	}
	
	public NodeSet getProperty(String name){
		return properties.get(name); 
	}
	
}