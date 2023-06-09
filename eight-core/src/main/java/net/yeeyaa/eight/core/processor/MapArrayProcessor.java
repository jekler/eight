package net.yeeyaa.eight.core.processor;

import java.util.List;
import java.util.Map;

import net.yeeyaa.eight.IProcessor;


public class MapArrayProcessor implements IProcessor<Map<Object, Object>, Object[]> {
	protected List<Object> conditions;
	protected IProcessor<Object, Object> processor;
	protected Integer index = -1;
	
	public void setIndex(Integer index) {
		if(index != null && index >= 0) this.index = index;
	}
	
	public void setConditions(List<Object> conditions) {
		this.conditions = conditions;
	}

	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	@Override
	public Object[] process(Map<Object, Object> in) {
		Object[] ret = null;
		if(in != null){
			ret = new Object[conditions == null ? 1 : 1 + conditions.size()];
			if(index < 0 || index > ret.length - 1) index = ret.length - 1;
			if(conditions != null && conditions.size() > 0) for(int i = 0, j = 0; i < conditions.size(); i++, j++) {
				if(j == index) j++;
 				ret[j] = processor == null ? in.remove(conditions.get(i)) : processor.process(in.remove(conditions.get(i)));
			}
			ret[index] = in;
		}
		return ret;
	}
}
