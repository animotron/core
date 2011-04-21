package org.animotron.exist;

import java.util.Iterator;
import java.util.Vector;

import org.exist.collections.Collection;
import org.exist.dom.DocumentSet;
import org.exist.dom.NodeSet;
import org.exist.dom.StoredNode;
import org.exist.numbering.NodeId;
import org.exist.xquery.Cardinality;
import org.exist.xquery.XPathException;
import org.exist.xquery.value.AtomicValue;
import org.exist.xquery.value.Item;
import org.exist.xquery.value.MemoryNodeSet;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.Type;
import org.exist.xquery.value.ValueSequence;

public class AnimoSequence implements Sequence {
	
	
	private Vector <Sequence> vector = new Vector <Sequence> ();
	private Sequence items = Sequence.EMPTY_SEQUENCE;
	private int itemType = Type.ANY_TYPE;
	private int size = 0;
	private boolean sizeChanged = false;
	
	public Sequence getFirst(){
		return vector.isEmpty() ? Sequence.EMPTY_SEQUENCE : vector.get(0);
	};
	
	public Sequence getLast(){
		return vector.isEmpty() ? Sequence.EMPTY_SEQUENCE : vector.get(vector.size() - 1);
	};
	
	private void reset(int type){
		if (itemType == type)
            return;
        else if (itemType == Type.ANY_TYPE)
            itemType = type;
        else
            itemType = Type.getCommonSuperType(type, itemType);
		sizeChanged = true;
		items = null;
	}
	
	public Sequence getItems() throws XPathException{
		if (items == null){
			items = new ValueSequence();
			for (Sequence i : vector){
				items.addAll(i);
			}
		}
		return items;
	}
	
	public void push(Sequence seq) throws XPathException{
		if (seq.isEmpty())
			return;
		vector.insertElementAt(seq, 0);
		reset(seq.getItemType());
	}
	
	public void add(Item item) throws XPathException {
		if (vector.isEmpty()){
			Sequence seq = new ValueSequence();
			seq.add(item);
			vector.add(seq);
		} else {
			getFirst().add(item);
		}
		reset(item.getType());
	}

	public void addAll(Sequence other) throws XPathException {
		if (other.isEmpty())
			return;
		if (vector.isEmpty())
			vector.add(other);
		else
			getFirst().addAll(other);
		reset(other.getItemType());
	}

	public int getItemType() {
		return itemType;
	}

	public SequenceIterator iterate() throws XPathException {
		return new AnimoSequenceIterator();
	}

	public SequenceIterator unorderedIterator() throws XPathException {
		return iterate();
	}

	public int getItemCount() {
		if (sizeChanged){
			size = 0;
			for (Sequence i : vector){
				size += i.getItemCount();
			}
			sizeChanged = false;
		}
		return size;
	}

	public int getSequenceCount() {
		return vector.size();
	}

	public boolean isEmpty() {
		return vector.isEmpty();
	}

	public boolean hasOne() {
		return vector.size() == 1 && getFirst().hasOne();
	}

	public boolean hasMany() {
		return vector.size() > 1 || getFirst().hasMany();
	}

	public void removeDuplicates() {
		for (Sequence i : vector) {
			i.removeDuplicates();
		}
	}

	public int getCardinality() {
		if (isEmpty())
			return Cardinality.EMPTY;
		if (hasOne())
			return Cardinality.EXACTLY_ONE;
		if (hasMany())
			return Cardinality.ONE_OR_MORE;
		throw new IllegalArgumentException("Illegal argument");
	}

	public Item itemAt(int pos) {
		Sequence seq = null;
		int index = pos;
		for (Sequence i : vector){
			seq = i;
			pos -=  i.getItemCount();
			if (pos <= 0){
				break;
			}
			index = pos;
		}
		return seq == null ? null : seq.itemAt(index);
	}

	public AtomicValue convertTo(int requiredType) throws XPathException {
		return getItems().convertTo(requiredType);
	}

	public String getStringValue() throws XPathException {
		return getItems().getStringValue();
	}

	public boolean effectiveBooleanValue() throws XPathException {
		return getItems().effectiveBooleanValue();
	}

	public NodeSet toNodeSet() throws XPathException {
		return getItems().toNodeSet();
	}

	public MemoryNodeSet toMemNodeSet() throws XPathException {
		return getItems().toMemNodeSet();
	}

	public DocumentSet getDocumentSet() {
		try {
			return getItems().getDocumentSet();
		} catch (XPathException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public Iterator<Collection> getCollectionIterator() {
		try {
			return getItems().getCollectionIterator();
		} catch (XPathException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public int conversionPreference(Class<?> javaClass) {
		try {
			return getItems().conversionPreference(javaClass);
		} catch (XPathException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return 0;
	}

	public Object toJavaObject(Class<?> target) throws XPathException {
		return getItems().toJavaObject(target);
	}

	public boolean isCached() {
		return false;
	}

	public void setIsCached(boolean cached) {
		//
	}

	public void clearContext(int contextId) throws XPathException {
		//
	}

	public void setSelfAsContext(int contextId) throws XPathException {
		//
	}

	public boolean isPersistentSet() {
		return false;
	}

	public void nodeMoved(NodeId oldNodeId, StoredNode newNode) {
		// 
	}

	public boolean isCacheable() {
		//
		return false;
	}

	public int getState() {
		//
		return 0;
	}

	public boolean hasChanged(int previousState) {
		return false;
	}
	
	private class AnimoSequenceIterator implements SequenceIterator {
		
		private Iterator <Sequence> pos = vector.iterator(); 
		private SequenceIterator cur;
		
		public AnimoSequenceIterator() {
			try {
				cur = getFirst().unorderedIterator();
			} catch (XPathException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		public boolean hasNext() {
		   return cur.hasNext() || pos.hasNext();
		}

		public Item nextItem() {
			if (cur.hasNext())
				return cur.nextItem();
			try {
				cur = pos.next().unorderedIterator();
			} catch (XPathException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return cur.nextItem();
		}

	}


}
