package net.yeeyaa.eight.client.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.client.data.ClientMsg;


public class ReunmarshalProcessor implements IProcessor<ClientMsg, ClientMsg>{
	protected IProcessor<String, ClientMsg> unmarshaller;
	protected String idprefix = "re:";
	
	public void setIdprefix(String idprefix) {
		this.idprefix = idprefix;
	}

	public void setUnmarshaller(IProcessor<String, ClientMsg> unmarshaller) {
		this.unmarshaller = unmarshaller;
	}
	
	@Override
	public ClientMsg process(ClientMsg msg) {
		if(msg != null && msg.id != null && msg.id.indexOf(idprefix) == 0 && msg.content instanceof String)
			return unmarshaller.process((String)msg.content);
		else return msg;
	}
}
