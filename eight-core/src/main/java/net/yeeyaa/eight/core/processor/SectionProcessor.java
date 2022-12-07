package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class SectionProcessor implements IProcessor<Object, Object>{
	protected ThreadLocal<BeanHolder> beanHolder = new ThreadLocal<BeanHolder>();
	
	protected ThreadLocal<BeanHolder> getBeanHolder() {
		return beanHolder;
	}

	public class BeanHolder implements IProcessor<Object, Object>{//main beanHolder for section
		protected IProcessor<Object, Object> beanHolder;//BeanHolder + SingletonProcessor = section
		
		public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
			this.beanHolder = beanHolder;
		}

		@Override
		public Object process(Object instance) { //can also use for spring factory method to product section bean group
			BeanHolder tmp = getBeanHolder().get(); //cache stack
			getBeanHolder().set(this);
			Object ret = beanHolder.process(instance);
			getBeanHolder().set(tmp);  //push stack back
			return ret;
		}
	}
	
	@Override
	public Object process(Object instance) { //main factory for callback
		BeanHolder beanHolder = this.beanHolder.get();
		if(beanHolder == null) return null;
		else return beanHolder.process(instance);
	}
}