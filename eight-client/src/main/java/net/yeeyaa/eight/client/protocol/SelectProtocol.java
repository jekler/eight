package net.yeeyaa.eight.client.protocol;

import java.util.Collections;
import java.util.Set;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.client.data.ClientMsg;


public class SelectProtocol<T> implements IProcessor<ClientMsg, ClientMsg> {
	protected IProcessor<ClientMsg, ClientMsg> text;
	protected IProcessor<ClientMsg, ClientMsg> stream;
	protected Set<String> except = Collections.EMPTY_SET;
	
	public void setText(IProcessor<ClientMsg, ClientMsg> text) {
		this.text = text;
	}

	public void setStream(IProcessor<ClientMsg, ClientMsg> stream) {
		this.stream = stream;
	}

	public void setExcept(Set<String> except) {
		if (except != null) this.except = except;
	}

	@Override
	public ClientMsg process(ClientMsg msg) {
		try{
			if (msg.content instanceof IProcessor && !except.contains(msg.name)) return stream.process(msg);
			else return text.process(msg);
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
}
