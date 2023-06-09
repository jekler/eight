package net.yeeyaa.eight.core.processor;

import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class ClassForkProcessor implements IProcessor<Object, Object>{
	protected Map<Class<?>, IProcessor<Object, Object>> processors;
	protected IProcessor<Object, Object> defaultProcessor;
	protected Boolean equal = true;
	
	public void setEqual(Boolean equal) {
		if (equal != null) this.equal = equal;
	}

	public void setProcessors(Map<Class<?>, IProcessor<Object, Object>> processors) {
		this.processors = processors;
	}

	public void setDefaultProcessor(IProcessor<Object, Object> defaultProcessor) {
		this.defaultProcessor = defaultProcessor;
	}

	@Override
	public Object process(Object instance) {
		if (instance != null) {
			IProcessor<Object, Object> processor = null;
			if (equal) processor = processors.get(instance.getClass());
			else for (Entry<Class<?>, IProcessor<Object, Object>> entry : processors.entrySet()) {
				if (entry.getKey().isAssignableFrom(instance.getClass())) processor = entry.getValue();
				break;
			}
			if (processor == null) processor = defaultProcessor;
			if (processor == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
			else return processor.process(instance);
		}
		return null;
	}
}
