package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class SystemLResource<K> implements IListableResource<K, String>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			return new Long(System.getProperties().size());
		}
	};

	public void setProperties(IListable<Object, String> resource) {
		Map<Object[], String> props= resource.all();
		Properties properties = new Properties();
		if (props != null && ! props.isEmpty()) for (Entry<Object[], String> entry : props.entrySet()) 
			if (entry.getKey() != null && entry.getKey().length > 0 && entry.getKey()[0] != null && entry.getValue() != null) 
				properties.put(entry.getKey()[0].toString(), entry.getValue());
		System.setProperties(properties);
	}
	
	public void setResource(IListable<Object, String> resource) {
		Map<Object[], String> props= resource.all();
		if (props != null && ! props.isEmpty()) for (Entry<Object[], String> entry : props.entrySet()) 
			if (entry.getKey() != null && entry.getKey().length > 0 && entry.getKey()[0] != null) 
				if (entry.getValue() == null) System.clearProperty(entry.getKey()[0].toString());
				else System.setProperty(entry.getKey()[0].toString(), entry.getValue());
	}
	
	@Override
	public String find(K ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) return System.getProperty(paras[0].toString());
		else return null;
	}

	@Override
	public <P> P store(String value, K ... paras) {
		if(paras != null && paras.length > 0  && paras[0] != null) System.setProperty(paras[0].toString(), value);
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 0  && paras[0] != null) System.clearProperty(paras[0].toString());
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		System.setProperties(null);
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Enumeration<Object> c = System.getProperties().elements();
		Collection ls = new LinkedList<K[]>();
		while (c.hasMoreElements()) {
			Object o = c.nextElement();
			Object[] key = PlatformUtil.newArrayOf(1, o);
			key[0] = o;
			ls.add(key);
		}
		return ls;
	}

	@Override
	public Map<K[], String> all(K... paras) {
		Map map = new HashMap<K[], String>();
		for(Entry<Object, Object> o : System.getProperties().entrySet()) {
			Object[] key = PlatformUtil.newArrayOf(1, o.getKey());
			key[0] = o.getKey();
			map.put(key, o.getValue());
		}
		return map;
	}
	
	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}
}
