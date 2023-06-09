package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class BeanProcessor<T> implements IProcessor<T[], Object> {
	protected IProcessor<Object, Object> beanHolder;
	protected Boolean returnAny = true;
	protected Boolean adapt = true;

	public void setAdapt(Boolean adapt) {
		if(adapt != null) this.adapt = adapt;
	}

	public void setReturnAny(Boolean returnAny) {
		if(returnAny != null) this.returnAny = returnAny;
	}

	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	@Override
	public Object process(T[] paras) {
		if(paras != null && paras.length > 0){
			Object o = beanHolder.process(paras[0]);
			if(o instanceof IProcessor){
				T[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
				for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 1];
				Object newo;
				if(paras.length > 1 || !adapt) newo = ((IProcessor)o).process(paras);
				else newo = ((IProcessor)o).process(paras[0]);
				return newo;
			}else if(o instanceof IInputResource){
				T[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
				for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 1];
				Object newo;
				if(paras.length > 1 || !adapt) newo = ((IInputResource)o).find(paras);
				else newo = ((IInputResource)o).find(paras[0]);
				return newo;
			}else if(returnAny) return o;
		}
		return null;
	}
}
