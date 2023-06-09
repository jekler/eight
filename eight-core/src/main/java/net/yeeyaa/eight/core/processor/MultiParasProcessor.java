package net.yeeyaa.eight.core.processor;

import java.util.List;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class MultiParasProcessor implements IProcessor<Object[], Object[]> {
	protected List<IProcessor<Object, Object>> processors;

	public void setProcessors(List<IProcessor<Object, Object>> processors) {
		this.processors = processors;
	}

	@Override
	public Object[] process(Object[] instance) {
		Object[] ret = new Object[instance == null ? 0: instance.length];
		if(instance != null && instance.length > 0 && processors != null && processors.size() == instance.length) for(int i = 0; i < instance.length; i ++){
			ret[i] = processors.get(i).process(instance[i]);
		}else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
		return ret;
	}
}
