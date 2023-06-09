package net.yeeyaa.eight.core.processor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;


public class SplitProcessor implements IProcessor<Object, Object>, IReadonlyListable<Object, Object> {
	protected String regex="\\|";
	protected Boolean mode;
	protected Map<Object, Object> resource = Collections.EMPTY_MAP;
	protected IProcessor<Object, Object> key;
	protected IProcessor<Object, Object> value;
	
	public SplitProcessor() {}

	public SplitProcessor(String regex) {
		this.regex = regex;
	}

	public SplitProcessor(IProcessor<Object, Object> key, IProcessor<Object, Object> value) {
		this.key = key;
		this.value = value;
	}
	
	public SplitProcessor(String regex, IProcessor<Object, Object> key, IProcessor<Object, Object> value) {
		this.regex = regex;
		this.key = key;
		this.value = value;
	}
	
	public void setMode(Boolean mode) {
		this.mode = mode;
	}

	public void setResource(String in) {
		if (in != null && in.trim().length() > 0) {
			String[] pairs = in.toString().split(regex + regex);
			HashMap<Object, Object> map = new HashMap<Object, Object>(pairs.length * 2);
			for (String pair : pairs) {
				String[] kv = pair.split(regex);
				if (kv.length > 1) {
					Object k = key == null ? kv[0] : key.process(kv[0]);
					Object v = value == null ? kv[1] : value.process(kv[1]);
					map.put(k, v);
				}
			}
			resource = map;
		}
	}
	
	@Override
	public Object process(Object in) {
		if(in == null) return null;
		else if (mode == null) {
			SplitProcessor ret = new SplitProcessor(regex, key, value);
			ret.setResource(in.toString());
			return ret;
		} else if (mode) {
			String[] pairs = in.toString().split(regex + regex);
			HashMap<String, String> ret = new HashMap<String, String>(pairs.length * 2);
			for (String pair : pairs) {
				String[] kv = pair.split(regex);
				if (kv.length > 1) ret.put(kv[0], kv[1]);
			}
			return ret;
		} else return in.toString().split(regex);
	}

	@Override
	public Object find(Object... paras) {
		return resource.get(paras[0]);
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		ArrayList<Object[]> ret = new ArrayList<Object[]>(resource.size());
		for (Object k : resource.keySet()) ret.add(new Object[]{k});
		return ret;
	}

	@Override
	public Map<Object[], Object> all(Object... paras) {
		HashMap<Object[], Object> ret = new HashMap<Object[], Object>(resource.size() * 2);
		for (Entry<Object, Object> entry : resource.entrySet()) ret.put(new Object[]{entry.getKey()}, entry.getValue());
		return ret;
	}
}
