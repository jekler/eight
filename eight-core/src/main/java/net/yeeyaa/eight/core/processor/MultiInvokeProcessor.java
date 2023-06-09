package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class MultiInvokeProcessor implements IProcessor<Object[], Object> {
	protected IProcessor<Object, Object> processor;
	protected Boolean compatible = false;
	
	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setCompatible(Boolean compatible) {
		if(compatible != null) this.compatible = compatible;
	}

	@Override
	public Object process(Object[] instance) {
		IProcessor<Object, Object> processor = this.processor;
		Object ret = null;
		if(instance != null && instance.length > 0) for(int i = 0; i < instance.length; i ++){
			ret = processor.process(instance[i]);
			if(ret instanceof IProcessor) processor = (IProcessor<Object, Object>) ret;
			else if(i < instance.length - 1) if(compatible) break;
			else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
		}else if(!compatible) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
		return ret;
	}
}
