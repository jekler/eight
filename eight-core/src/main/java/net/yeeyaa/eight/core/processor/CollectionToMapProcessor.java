package net.yeeyaa.eight.core.processor;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class CollectionToMapProcessor implements IProcessor<Object, Map<Object, Object>> {
	protected Boolean nullable = true;
	
	public void setNullable(Boolean nullable) {
		if (nullable != null) this.nullable = nullable;
	}

	@Override
	public Map<Object, Object> process(Object in) {	
		if(in != null){
			if(in.getClass().isArray()) in = TypeConvertor.asCollection(in);
			if(Collection.class.isInstance(in)) {
				Map<Object, Object> hm = new LinkedHashMap<Object, Object>();
				for(Object entry : (Collection)in) if(Entry.class.isInstance(entry)) {
					if(nullable || ((Entry)entry).getValue() != null) hm.put(((Entry)entry).getKey(), ((Entry)entry).getValue());
				} else return null;
				return hm;
			}
		}
		return null;
	}
}
