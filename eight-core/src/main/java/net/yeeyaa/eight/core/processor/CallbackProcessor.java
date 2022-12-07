package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class CallbackProcessor implements IProcessor<Object, Object> {
	protected ThreadLocal<Object> local = new ThreadLocal<Object>();
	protected IProcessor<Object, Object> processor; //usually beanholder
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

	public Object getValue() { //beanHolder (usually platformBean) gets parameters from local thread 
		if (valuePrcoessor == null) return local.get();
		else return valuePrcoessor.process(local.get());
	}

	@Override
	public Object process(Object paras) {//when paras is null, this processor equals a proxy processor which convert parafied processor to a null parameter processor
		this.local.set(paras); //set paras
		Object ret = processor.process(this.paras); //gets paras and generators bean
		this.local.remove(); //clear temp paras, prevent memory leak
		return ret;
	}
}
