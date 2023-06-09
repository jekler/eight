package net.yeeyaa.eight.core.processor;

import java.util.Collection;
import java.util.HashSet;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.storage.Storage.Method;


public class StorageChangeProcessor implements IProcessor<Collection<IExtendable<Object>>, Object>{
	protected HashSet<String> storages = new HashSet<String>();
    protected IProcessor<Object, String> nameProcessor;
    protected IProcessor<Collection<IExtendable<Object>>, Object> changeProcessor;
    protected IProcessor<Collection<IExtendable<Object>>, Object> noChangeProcessor;
    protected IProcessor<Collection<IExtendable<Object>>, Object> defaultProcessor;
    protected Boolean change; 

    public void setDefaultProcessor(IProcessor<Collection<IExtendable<Object>>, Object> defaultProcessor) {
		this.defaultProcessor = defaultProcessor;
	}

	public void setNameProcessor(IProcessor<Object, String> nameProcessor) {
		this.nameProcessor = nameProcessor;
	}
    
    public void setNoChangeProcessor(IProcessor<Collection<IExtendable<Object>>, Object> noChangeProcessor) {
		this.noChangeProcessor = noChangeProcessor;
	}

	public void setChangeProcessor(IProcessor<Collection<IExtendable<Object>>, Object> changeProcessor) {
		this.changeProcessor = changeProcessor;
	}

	public void setChange(Boolean change) {
		this.change = change;
	}

	protected String getName(IExtendable<Object> storage){
    	if(storage != null) if(nameProcessor == null) {
    		Object key = storage.extend(Method.key);
    		if(key instanceof String) return (String)key;
    		else if(key instanceof Object[] && ((Object[])key).length > 0 && ((Object[])key)[((Object[])key).length - 1] instanceof String)  
    			return (String)((Object[])key)[((Object[])key).length - 1];
    		else if(key instanceof Collection){
    			Object[] ks = ((Collection<?>) key).toArray();
    			if( ((Object[])ks).length > 0 && ((Object[])ks)[((Object[])ks).length - 1] instanceof String) return (String)((Object[])ks)[((Object[])ks).length - 1];
    		}else if(key != null) return key.toString();
    	}else return nameProcessor.process(storage.extend(Method.key));
    	return null;
    }
	
	@Override
	public Object process(Collection<IExtendable<Object>> instance) {
		if (instance != null && instance.size() > 0 && (changeProcessor != null || noChangeProcessor != null)) synchronized(this) {
			HashSet<IExtendable<Object>> changes = new HashSet<IExtendable<Object>>();
			HashSet<String> tmp = new HashSet<String>();
			for (IExtendable<Object> storage : instance) {
				String name = getName(storage);
				if (name != null) if (storages.contains(name)) {
					if (!Boolean.FALSE.equals(change) || Boolean.TRUE.equals(storage.extend(Method.exists))) changes.add(storage);
				} else {
					if (Boolean.TRUE.equals(change)) changes.add(storage);
					tmp.add(name);
				}
			}
			storages.addAll(tmp);
			if (changes.size() > 0) { if (changeProcessor != null) return changeProcessor.process(changes); }
			else if(noChangeProcessor != null) return noChangeProcessor.process(instance);
		}
		if (defaultProcessor != null) return defaultProcessor.process(instance);
		else return null;
	}
}
