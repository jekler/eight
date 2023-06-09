package net.yeeyaa.eight.core;

import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IThing;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.IUniversal;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

public class PlatformUniversal<K, V, U extends IListableResource<K, V>, T, R> implements IUniversal<K, V, U, T, R>{
	protected IProcessor<Object, Object> beanHolder; 
	protected Object bean;
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setBean(Object bean) {
		this.bean = bean;
	}

	@Override
	public R process(T object) {
		Object o = beanHolder.process(bean);
		if(o instanceof IProcessor) return ((IProcessor<T, R>)o).process(object);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}
	
	@Override
	public R perform(K name, V parameters) {
		Object o = beanHolder.process(bean);
		if(o instanceof IBiProcessor) return ((IBiProcessor<K, V, R>)o).perform(name, parameters);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public R operate(T first, K second, V third) {
		Object o = beanHolder.process(bean);
		if(o instanceof ITriProcessor) return ((ITriProcessor<T, K, V, R>)o).operate(first, second, third);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public V find(K... paras) {
		Object o = beanHolder.process(bean);
		if(o instanceof IInputResource) return ((IInputResource<K, V>)o).find(paras);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public <P> P store(V value, K... paras) {
		Object o = beanHolder.process(bean);
		if(o instanceof IOutputResource) return ((IOutputResource<K, V>)o).<P>store(value, paras);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public <P> P discard(K... paras) {
		Object o = beanHolder.process(bean);
		if(o instanceof IOutputResource) return ((IOutputResource<K, V>)o).<P>discard(paras);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public <P> P empty(K... paras) {
		Object o = beanHolder.process(bean);
		if(o instanceof IOutputResource) return ((IOutputResource<K, V>)o).<P>empty(paras);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Object o = beanHolder.process(bean);
		if(o instanceof IListable) return ((IListable<K, V>)o).keys(paras);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Object o = beanHolder.process(bean);
		if(o instanceof IListable) return ((IListable<K, V>)o).all(paras);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	public R execute(IProcessor<U, R> processor) {
		Object o = beanHolder.process(bean);
		if(o instanceof ITransaction) return ((ITransaction<K, V, U, R>)o).execute(processor);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}
	
	@Override
	public <O> O realObject() {
		Object o = beanHolder.process(bean);
		if(o instanceof IUniversal) return ((IUniversal<K, V, U, T, R>)o).<O>realObject();
		else return (O) o;
	}

	@Override
	public <N> N extend(T method) {
		Object o = beanHolder.process(bean);
		if(o instanceof IExtendable) return ((IExtendable<T>)o).<N>extend(method);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}
	
	@Override
	public Collection<T> methods() {
		Object o = beanHolder.process(bean);
		if(o instanceof IExtendable) return ((IExtendable<T>)o).methods();
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}

	@Override
	public <L> L present(Class<L> clazz) {
		Object o = beanHolder.process(bean);
		if(o instanceof IThing) return ((IThing)o).present(clazz);
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}
}
