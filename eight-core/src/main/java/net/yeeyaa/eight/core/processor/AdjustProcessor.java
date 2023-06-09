package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;

public class AdjustProcessor implements IBiProcessor<Object, Object, Object>, ITriProcessor<Object, Object, Object, Object> {
	protected IProcessor<Object, Object> processor;
	protected IBiProcessor<Object, Object, Object> bi;
	protected Boolean adjust; 
	
	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setBi(IBiProcessor<Object, Object, Object> bi) {
		this.bi = bi;
	}

	public void setAdjust(Boolean adjust) {
		this.adjust = adjust;
	}

	@Override
	public Object operate(Object first, Object second, Object third) {
		if (processor != null) if (adjust == null) return processor.process(third);
		else if (adjust) return processor.process(second);
		else return processor.process(first);
		else if (bi != null) if (adjust == null) return bi.perform(first, second);
		else if (adjust) return bi.perform(first, third);
		else return bi.perform(second, third);
		else return null;
	}

	@Override
	public Object perform(Object first, Object second) {
		if (adjust == null) return processor.process(second);
		else return processor.process(first);
	}
}
