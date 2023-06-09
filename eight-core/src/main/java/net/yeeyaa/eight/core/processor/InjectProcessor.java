package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class InjectProcessor<R> implements IProcessor<R, R> {
	protected R inject;
	protected ThreadLocal<R> holder = new ThreadLocal<R>();
	protected Boolean thread = false;
	
	public void setInheritable(Boolean inheritable) {
		if(Boolean.TRUE.equals(inheritable)) holder = new InheritableThreadLocal<R>();
	}

	public void setThread(Boolean thread) {
		if(thread != null) this.thread = thread;
	}

	public void setInject(R inject) {
		this.inject = inject;
	}

	@Override
	public R process(R in) {
		if(thread) holder.set(in);
		else inject = in;
		return in;
	}
	
	public R get(){
		if(thread) return holder.get();
		else return inject;
	}
	
	public class Get implements IProcessor<Object, R>{
		@Override
		public R process(Object instance) {
			return get();
		}
	}
}
