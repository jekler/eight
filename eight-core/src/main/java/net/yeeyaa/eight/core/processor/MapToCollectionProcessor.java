package net.yeeyaa.eight.core.processor;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;


public class MapToCollectionProcessor implements IProcessor<Map<?, ?>, Object> {
	protected Boolean isList = true;
	
	public void setIsList(Boolean isList) {
		if (isList != null) this.isList = isList;
	}

	@Override
	public Object process(Map<?, ?> in) {	
		if(in != null) if(isList) return in.entrySet();
			else return in.entrySet().toArray();
		else return null;
	}
}
