package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;

public class AdjustProxy implements IBiProcessor<Object, Object, Object>, ITriProcessor<Object, Object, Object, Object> {
	protected IProcessor<Object, IProcessor<Object, Object>> processors;
	protected IProcessor<Object, IBiProcessor<Object, Object, Object>> bis;

	public void setProcessors(IProcessor<Object, IProcessor<Object, Object>> processors) {
		this.processors = processors;
	}

	public void setBis(IProcessor<Object, IBiProcessor<Object, Object, Object>> bis) {
		this.bis = bis;
	}

	@Override
	public Object perform(Object first, Object second) {
		IProcessor<Object, Object> processor = processors.process(first);
		if (processor != null) return processor.process(second);
		else return null;
	}

	@Override
	public Object operate(Object first, Object second, Object third) {
		IBiProcessor<Object, Object, Object> bi = bis.process(first);
		if (bi != null) return bi.perform(second, third);
		else return null;
	}
}
