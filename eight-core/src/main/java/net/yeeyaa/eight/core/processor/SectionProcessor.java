package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class SectionProcessor implements IProcessor<Object, Object>{
	protected ThreadLocal<BeanHolder> beanHolder = new ThreadLocal<BeanHolder>();
	
	protected ThreadLocal<BeanHolder> getBeanHolder() {
		return beanHolder;
	}

	public class BeanHolder implements IProcessor<Object, Object>{
		protected IProcessor<Object, Object> beanHolder;
		
		public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
			this.beanHolder = beanHolder;
		}

		@Override
		public Object process(Object instance) { 
			BeanHolder tmp = getBeanHolder().get(); 
			getBeanHolder().set(this);
			Object ret = beanHolder.process(instance);
			getBeanHolder().set(tmp);  
			return ret;
		}
	}
	
	@Override
	public Object process(Object instance) { 
		BeanHolder beanHolder = this.beanHolder.get();
		if(beanHolder == null) return null;
		else return beanHolder.process(instance);
	}
}
