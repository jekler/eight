package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class CallbackProcessor implements IProcessor<Object, Object> {
	protected ThreadLocal<Object> local = new ThreadLocal<Object>();
	protected IProcessor<Object, Object> processor; 
	protected Object paras;
	protected IProcessor<Object, Object> valuePrcoessor;

	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setValuePrcoessor(IProcessor<Object, Object> valuePrcoessor) {
		this.valuePrcoessor = valuePrcoessor;
	}

	public void setParas(Object paras) {
		this.paras = paras;
	}

	public Object getValue() { 
		if (valuePrcoessor == null) return local.get();
		else return valuePrcoessor.process(local.get());
	}

	@Override
	public Object process(Object paras) {
		this.local.set(paras); 
		Object ret = processor.process(this.paras); 
		this.local.remove(); 
		return ret;
	}
}
