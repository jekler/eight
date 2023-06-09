package net.yeeyaa.eight.common.meta;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.common.meta.PlatformVariable.InitValue;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class MetaGcProcessor<K> implements IProcessor<Object, Object>{
	protected IProcessor<Collection<K[]>, Collection<K[]>> root;
	protected IListableResource<K, Object> resource;
	protected K[] key;
	
	public void setRoot(IProcessor<Collection<K[]>, Collection<K[]>> root) {
		this.root = root;
	}

	public void setResource(IListableResource<K, Object> resource) {
		this.resource = resource;
	}

	public void setKey(K[] key) {
		this.key = key;
	}

	public void destroy(){
		if(key == null) resource.empty();
		else resource.empty(key);
	}
	
	protected void search(Object entity, Map<Collection<K>, K[]> keyMap, Map<K[], Object> map, Set<K[]> set){
		if(entity != null) if(entity instanceof InitValue) {
			Object[] key = ((InitValue) entity).getKey();
			if(key != null) {
				K[] k = keyMap.get(Arrays.asList(key));
				if(k != null) {
					Object instance = map.get(k);
					if(instance != null) {
						set.add(k);
						search(instance, keyMap, map, set);
					}
				}
			}
		} else if(entity instanceof BeanMeta) search(((BeanMeta)entity).getInitValue(), keyMap, map, set);
		else if(entity instanceof Object[]) for (Object o : (Object[]) entity) search(o, keyMap, map, set);
		else if(entity instanceof Collection) for (Object o : (Collection<Object>) entity) search(o, keyMap, map, set);
		else if(entity instanceof Map) for(Object o : ((Map<Object, Object>) entity).values()) search(o, keyMap, map, set);
	}
	
	@Override
	public synchronized Object process(Object instance) {
		Map<K[], Object> map = key == null ? resource.all() : resource.all(key);
		if(map != null && map.size() > 0) {
			Map<Collection<K>, K[]> keyMap = new HashMap<Collection<K>, K[]>();
			for(K[] key : map.keySet()) keyMap.put(Arrays.asList(key), key);
			Collection<K[]> rootKeys = root.process(map.keySet());
			HashSet<K[]> set = new HashSet<K[]>();
			if(rootKeys != null && rootKeys.size() > 0) {
				for(K[] key : rootKeys) search(map.get(key), keyMap, map, set);
				set.addAll(rootKeys);
			}
			if(set.size() > 0) for(K[] key : set) map.remove(key);
			if(map.size() > 0) for(K[] key : map.keySet()) resource.discard(key);
		}
		return instance;
	}
	
	public class Destroy implements IProcessor<Object, Object>{
		@Override
		public Object process(Object instance) {
			destroy();
			return instance;
		}
	}
	
	public static class RootKeys<K> implements IProcessor<Collection<K[]>, Collection<K[]>>{
		protected Integer prefix = 0;
		protected Integer suffix = 0;
		protected Integer index = 0;
		protected IProcessor<String, Boolean> keyValidator;

		public void setIndex(Integer index) {
			if (index != null && index > 0) this.index = index;
		}

		public void setKeyValidator(IProcessor<String, Boolean> keyValidator) {
			this.keyValidator = keyValidator;
		}

		public void setPrefix(Integer prefix) {
			if(prefix != null && prefix > 0) this.prefix = prefix;
		}

		public void setSuffix(Integer suffix) {
			if(suffix != null && suffix > 0) this.suffix = suffix;
		}
		
		@Override
		public Collection<K[]> process(Collection<K[]> instance) {
			Collection<K[]> ret = new HashSet<K[]>();
			if(instance != null && instance.size() > 0){
				ret.addAll(instance);
				for(K[] key : instance) if(key != null && key.length > index){
					String k = key[index] == null ? null : key[index].toString();
					if(k != null && k.length() > prefix + suffix) {
						k = k.substring(prefix, k.length() - suffix);
						if(keyValidator == null) try{
							TypeConvertor.strToUuid(k);
							ret.remove(key);
						} catch(Exception e){}
						else if(keyValidator.process(k)) ret.remove(key);
					}
				}
			}
			return ret;
		}
	}
}
