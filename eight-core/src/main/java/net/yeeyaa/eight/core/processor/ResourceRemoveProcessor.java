package net.yeeyaa.eight.core.processor;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class ResourceRemoveProcessor<K, V> implements IProcessor<Object, Collection<K[]>> {
	protected IListableResource<K, V> resource;
	protected Boolean isArray = false;
	protected Boolean reserve; 
	
	public void setReserve(Boolean reserve) {
		this.reserve = reserve;
	}

	public void setIsArray(Boolean isArray) {
		if(isArray != null) this.isArray = isArray;
	}

	public void setResource(IListableResource<K, V> resource) {
		this.resource = resource;
	}

	@Override
	public Collection<K[]> process(Object in) {	
		Collection<K[]> ret = resource.keys();
		if(in != null) if(isArray){
			HashMap<List<K>, K[]> map = new HashMap<List<K>, K[]>(ret.size() * 2);
			for(K[] key : ret) if(key != null && key.length > 0) map.put(Arrays.asList(key), key);
			if(in.getClass().isArray()) in = TypeConvertor.asCollection(in);
			if(in instanceof Collection) for(Object o : (Collection) in) {
				K[] key;
				if(o instanceof Object[]) key = map.get(Arrays.asList((K[])o));
				else key = map.get(o);
				if(key != null){
					ret.remove(key);
					if(Boolean.FALSE.equals(reserve)) resource.discard(key);
				}
			} else {
				K[] key;
				if(in instanceof Object[]) key = map.get(Arrays.asList((K[])in));
				else key = map.get(in);
				if(key != null){
					ret.remove(key);
					if(Boolean.FALSE.equals(reserve)) resource.discard(key);
				}
			}
		}else{
			HashMap<K, K[]> map = new HashMap<K, K[]>(ret.size() * 2);
			for(K[] key : ret) if(key != null && key.length > 0) map.put(key[0], key);
			if(in.getClass().isArray()) in = TypeConvertor.asCollection(in);
			if(in instanceof Collection) for(Object o : (Collection) in){
				K[] key = map.get(o);
				if(key != null) {
					ret.remove(key);
					if(Boolean.FALSE.equals(reserve)) resource.discard(key);
				}
			}else{
				K[] key = map.get(in);
				if(key != null) {
					ret.remove(key);
					if(Boolean.FALSE.equals(reserve)) resource.discard(key);
				}
			}
		}
		if(Boolean.TRUE.equals(reserve)) for(K[] key : ret) resource.discard(key);
		return ret;
	}
}
