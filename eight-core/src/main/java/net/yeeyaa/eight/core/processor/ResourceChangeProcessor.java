package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;


public class ResourceChangeProcessor implements IProcessor<Map<Object[], Object>, Object>{
	protected volatile HashMap<Collection<Object>, Object> resources = new HashMap<Collection<Object>, Object>();
    protected IProcessor<Object, Object> digestProcessor;
    protected IProcessor<Object[], Boolean> comparator;
    protected IProcessor<Map<Object[], Object>, Object> changeProcessor;
    protected Boolean change; 
    protected Object delete;
    
	public void setDelete(Object delete) {
		this.delete = delete;
	}

	public void setDigestProcessor(IProcessor<Object, Object> digestProcessor) {
		this.digestProcessor = digestProcessor;
	}

	public void setChangeProcessor(IProcessor<Map<Object[], Object>, Object> changeProcessor) {
		this.changeProcessor = changeProcessor;
	}

	public void setChange(Boolean change) {
		this.change = change;
	}
	
	public void setComparator(IProcessor<Object[], Boolean> comparator) {
		this.comparator = comparator;
	}

	protected Boolean compare(Object left, Object right){
		if(left == null ? right == null : left.equals(right)) return true;
		else {
			if(left != null && right != null && left.getClass().isArray() && right.getClass().isArray()){
				int length = Array.getLength(left);
				if(length == Array.getLength(right)) for(int i = 0; i < length; i++) { if(!compare(Array.get(left, i), Array.get(right, i))) return false; }
				else return false;
				return true;
			}
			return false;
		}
	}
	
	@Override
	public Object process(Map<Object[], Object> instance) {
		if (instance != null && instance.size() > 0 && changeProcessor != null) synchronized(this) {
			HashMap<Object[], Object> changes = new HashMap<Object[], Object>();
			HashMap<Collection<Object>, Object> tmp = new HashMap<Collection<Object>, Object>();
			for (Entry<Object[], Object> entry : instance.entrySet()) {
				Collection<Object> key = Arrays.asList(entry.getKey());
				Object value = digestProcessor == null ? entry.getValue() : digestProcessor.process(entry.getValue());
				tmp.put(key, value);
				if (resources.containsKey(key)) {
					Object origin = resources.remove(key);
					if (comparator == null) {
						if (!compare(origin, value)) changes.put(entry.getKey(), entry.getValue());
					} else if (!comparator.process(new Object[]{origin, value})) changes.put(entry.getKey(), entry.getValue());
				} else if (Boolean.TRUE.equals(change)) changes.put(entry.getKey(), entry.getValue());
			}
			if (!Boolean.FALSE.equals(change)) for (Collection<Object> key : resources.keySet()) changes.put(key.toArray(), delete);
			resources = tmp;
			if (changes.size() > 0) return changeProcessor.process(changes);
		}
		return null;
	}
}
