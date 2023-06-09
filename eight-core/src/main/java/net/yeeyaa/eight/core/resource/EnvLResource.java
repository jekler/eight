package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class EnvLResource<K> implements IReadonlyListable<K, String>, IExtendable<Object>, IBiProcessor<IOutputResource<String, String>, String, IOutputResource<String, String>> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected String regex="\\|";
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			return new Long(System.getenv().size());
		}
	};
	
	public void setRegex(String regex) {
		if (regex != null) this.regex = regex;
	}
	
	@Override
	public String find(K ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) return System.getenv(paras[0].toString());
		else return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Set<String> c = System.getenv().keySet();
		Collection ls = new ArrayList<K[]>(c.size() * 2);
		for (String o : c) {
			Object[] key = PlatformUtil.newArrayOf(1, o);
			key[0] = o;
			ls.add(key);
		}
		return ls;
	}

	@Override
	public Map<K[], String> all(K... paras) {
		Map map = new HashMap<K[], String>(System.getenv().size() * 2);
		for(Entry<String, String> o : System.getenv().entrySet()) {
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

	@Override
	public IOutputResource<String, String> perform(IOutputResource<String, String> resource, String properties) {
    	if (properties != null && properties.trim().length() != 0) {
	    	String[] props = properties.split(regex+regex);
	    	for(String p : props) if (p.trim().length() > 0) {
	    		String[] ps = p.split(regex);
				String key = ps[0].trim();
				if(key.length() > 0 && ps.length > 1 && ps[1].trim().length() > 0) resource.store(ps[1].trim(), key);
	    	}
    	}
		return resource;
	}
}
