package net.yeeyaa.eight.client.protocol;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.client.data.ClientMsg;


public abstract class AbstractProtocol<T> implements IProcessor<ClientMsg, ClientMsg> {
	protected IProcessor<Object, T> marshaller;
	protected IProcessor<T, ClientMsg> unmarshaller;
	
	public void setMarshaller(IProcessor<Object, T> marshaller) {
		this.marshaller = marshaller;
	}
	
	public void setUnmarshaller(IProcessor<T, ClientMsg> unmarshaller) {
		this.unmarshaller = unmarshaller;
	}

	@Override
	public ClientMsg process(ClientMsg msg) {
		try{
			return unmarshaller.process(handle(marshaller.process(msg), msg));
		}catch(Exception e){
			if(e instanceof PlatformException) throw (PlatformException)e;
			else {
				DefaultError se = new DefaultError();
				se.setCate("ConnectServerError");
				se.setCode(999);
				se.setMessage(e.getMessage());
				throw new PlatformException(se);
			}
		}
	}

	public abstract T handle(T req, ClientMsg msg) throws Exception;
}
