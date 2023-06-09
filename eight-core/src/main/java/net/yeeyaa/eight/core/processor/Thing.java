package net.yeeyaa.eight.core.processor;

import java.util.HashMap;
import java.util.Map;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IListableTransaction;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.IThing;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.ITransactionResource;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IUniversal;


public class Thing implements IThing {
	protected Object proxy = this;
	protected final Map<Object, Object> map;
	
	public Thing() {
		map = new HashMap<Object, Object>();
	}
	
	public Thing(Map<Object, Object> map) {
		this.map = map;
	}

	public void setProxy(Object proxy) {
		this.proxy = proxy;
	}
	
	public void setProcessor(IProcessor<Object, Object> processor) {
		map.put(IProcessor.class, processor);
	}

	public void setBiProcessor(IBiProcessor<Object, Object, Object> biProcessor) {
		map.put(IBiProcessor.class, biProcessor);
	}

	public void setTriProcessor(ITriProcessor<Object, Object, Object, Object> triProcessor) {
		map.put(ITriProcessor.class, triProcessor);
	}

	public void setInputResource(IInputResource<Object, Object> inputResource) {
		map.put(IInputResource.class, inputResource);
	}

	public void setOutputResource(IOutputResource<Object, Object> outputResource) {
		map.put(IOutputResource.class, outputResource);
	}

	public void setListable(IListable<Object, Object> listable) {
		map.put(IListable.class, listable);
	}

	public void setResource(IResource<Object, Object> resource) {
		map.put(IResource.class, resource);
	}

	public void setReadonlyListable(IReadonlyListable<Object, Object> readonlyListable) {
		map.put(IReadonlyListable.class, readonlyListable);
	}

	public void setListableResource(IListableResource<Object, Object> listableResource) {
		map.put(IListableResource.class, listableResource);
	}

	public void setTransaction(ITransaction<Object, Object, IResource<Object, Object>, Object> transaction) {
		map.put(ITransaction.class, transaction);
	}

	public void setTransactionResource(ITransactionResource<Object, Object, IResource<Object, Object>, Object> transactionResource) {
		map.put(ITransactionResource.class, transactionResource);
	}

	public void setListableTransaction(IListableTransaction<Object, Object, IResource<Object, Object>, Object> listableTransaction) {
		map.put(IListableTransaction.class, listableTransaction);
	}

	public void setExtendable(IExtendable<Object> extendable) {
		map.put(IExtendable.class, extendable);
	}

	public void setUniversal(IUniversal<Object, Object, IListableResource<Object, Object>, Object, Object> universal) {
		map.put(IUniversal.class, universal);
	}
	
	@Override
	public <L> L present(Class<L> clazz) {
		L ret = (L) map.get(clazz);
		return ret == null ? (L) proxy : ret;
	}
}
