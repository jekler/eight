package net.yeeyaa.eight.core.processor;

import java.util.Collection;

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
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.ITransactionResource;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IUniversal;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class Extendable<M> extends Thing implements IExtendable<M> {
	public enum Method {
		processor(IProcessor.class),
		biProcessor(IBiProcessor.class),
		triProcessor(ITriProcessor.class),
		inputResource(IInputResource.class),
		outputResource(IOutputResource.class),
		listable(IListable.class),
		resource(IResource.class),
		readonlyListable(IReadonlyListable.class),
		listableResource(IListableResource.class),
		transaction(ITransaction.class),
		transactionResource(ITransactionResource.class),
		listableTransaction(IListableTransaction.class),
		extendable(IExtendable.class),
		universal(IUniversal.class);
		protected Class<?> clazz;
		
		private Method(Class<?> clazz){
			this.clazz = clazz;
		};
		
		public Class<?> getMethod() {
			return clazz;
		}
	}
	
	@Override
	public Collection<M> methods() {
		return (Collection)map.keySet();
	}

	@Override
	public <N> N extend(M method) {
		if (method != null) try {
			N ret = (N) map.get(method);
			if (ret == null) ret = (N) map.get(method instanceof Method ? ((Method) method).getMethod() : Method.valueOf(method.toString()).getMethod());
			return ret;
		} catch (Exception e) {
			try {
				return (N) map.get(Class.forName(method.toString()));
			} catch (Exception ex) {
				throw new PlatformException(PlatformError.ERROR_NOT_EXIST, ex.getMessage());
			}
		}
		return null;
	}
}
