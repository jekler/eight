package net.yeeyaa.eight.client.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.client.data.ClientMsg;


public class FilterProcessor implements IProcessor<ClientMsg, ClientMsg>{
	protected IProcessor<ClientMsg, ClientMsg> next;
	protected IProcessor<ClientMsg, ClientMsg> in;
	protected IProcessor<ClientMsg, ClientMsg> out;	
	
	public void setNext(IProcessor<ClientMsg, ClientMsg> next) {
		this.next = next;
	}

	public void setIn(IProcessor<ClientMsg, ClientMsg> in) {
		this.in = in;
	}

	public void setOut(IProcessor<ClientMsg, ClientMsg> out) {
		this.out = out;
	}

	@Override
	public ClientMsg process(ClientMsg msg) {
		if(out != null && msg != null) msg = out.process(msg);
		msg = next.process(msg);
		if(in != null && msg != null) msg = in.process(msg);
		return msg;
	}
}
