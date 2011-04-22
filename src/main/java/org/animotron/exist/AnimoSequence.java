package org.animotron.exist;

import java.util.Iterator;
import java.util.Vector;

import org.exist.collections.Collection;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.xquery.XPathException;
import org.exist.xquery.value.AbstractSequence;
import org.exist.xquery.value.Item;
import org.exist.xquery.value.MemoryNodeSet;
import org.exist.xquery.value.NodeValue;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.Type;
import org.exist.xquery.value.ValueSequence;

public class AnimoSequence extends AbstractSequence {

	private Vector <Sequence> vector = new Vector <Sequence> ();
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
	}
	
	public void push(Sequence seq) throws XPathException{
		if (seq.isEmpty())
			return;
		vector.insertElementAt(seq, 0);
		reset(seq.getItemType());
	}
	
	@Override
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

	public void removeDuplicates() {
		for (Sequence i : vector) {
			i.removeDuplicates();
		}
	}

	public NodeSet toNodeSet() throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

	public MemoryNodeSet toMemNodeSet() throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getItemType() {
		return itemType;
	}

	@Override
	public SequenceIterator iterate() throws XPathException {
		return new AnimoSequenceIterator();
	}

	@Override
	public SequenceIterator unorderedIterator() throws XPathException {
		return iterate();
	}

	@Override
	public Iterator<Collection> getCollectionIterator() {
		return new CollectionIterator();
	}

	@Override
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

	@Override
	public boolean isEmpty() {
		return vector.isEmpty();
	}

	@Override
	public boolean hasOne() {
		return vector.size() == 1 && getFirst().hasOne();
	}

	@Override
	public boolean hasMany() {
		return vector.size() > 1 || getFirst().hasMany();
	}
	
	@Override
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
	
    private class CollectionIterator implements Iterator<Collection> {

    	private Collection next = null;
    	private SequenceIterator i;
		
        CollectionIterator() {
        	try {
				i = iterate();
			} catch (XPathException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
            next();
        }

        public boolean hasNext() {
            return next != null;
        }

        public Collection next() {
        	
            Collection old = next;
            
            while (i.hasNext()) {
            	Item item = i.nextItem();
                if (Type.subTypeOf(item.getType(), Type.NODE)) {
                    NodeValue node = (NodeValue) item;
                    if (node.getImplementationType() == NodeValue.PERSISTENT_NODE) {
                        NodeProxy p = (NodeProxy) node;
                        if (!p.getDocument().getCollection().equals(old)) {
                            next = p.getDocument().getCollection();
                            break;
                        }
                    }
                }
            }
            return old;
        }

        public void remove() {
             // not needed
            throw new IllegalStateException();
        }
        
    }


}
