package net.yeeyaa.eight.client.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.client.data.ClientMsg;


public class RemarshalProcessor implements IProcessor<ClientMsg, ClientMsg>{
	protected String tag = ":";
	protected IProcessor<ClientMsg, String> marshaller;
	protected String idprefix = "re:";
	
	public void setIdprefix(String idprefix) {
		this.idprefix = idprefix;
	}

	public void setTag(String tag) {
		if(tag != null) this.tag = tag;
	}

	public void setMarshaller(IProcessor<ClientMsg, String> marshaller) {
		this.marshaller = marshaller;
	}

	@Override
	public ClientMsg process(ClientMsg msg) {
		if(msg != null && msg.name != null){
			String[] names = msg.name.split(tag);
			if(names.length > 1) {
				ClientMsg newmsg = msg.clone();
				msg.name = names[1];
				newmsg.content = marshaller.process(msg);
				newmsg.name = names[0];
				StringBuilder sb = new StringBuilder();
				sb.append(idprefix);
				if(newmsg.id != null) sb.append(newmsg.id);
				newmsg.id = sb.toString();
				return newmsg;
			}
		}
		return msg;
	}	
}
