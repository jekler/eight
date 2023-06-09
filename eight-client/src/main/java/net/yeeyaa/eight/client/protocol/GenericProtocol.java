package net.yeeyaa.eight.client.protocol;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.client.data.ClientMsg;


public class GenericProtocol<T>  extends AbstractProtocol<T>{
	protected IProcessor<T, T> next;
	
	public void setNext(IProcessor<T, T> next) {
		this.next = next;
	}

	@Override
	public T handle(T req, ClientMsg msg) throws Exception {
		return next.process(req);
	}
}
