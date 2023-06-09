package net.yeeyaa.eight.client;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.client.data.ClientMsg;


public class ServiceClient<K, V, R> implements IBiProcessor<K, V, R>, IProcessor<ClientMsg, R> {
	protected IProcessor<Void, String> tokenInput =  new DefaultToken();
	protected IProcessor<String, Void> tokenOutput = ((DefaultToken)tokenInput).new Output();
	protected IProcessor<ClientMsg, ClientMsg> protocol;
	protected K name;
	
	public void setTokenInput(IProcessor<Void, String> tokenInput) {
		this.tokenInput = tokenInput;
	}

	public void setTokenOutput(IProcessor<String, Void> tokenOutput) {
		this.tokenOutput = tokenOutput;
	}

	public String getToken() {
		return tokenInput == null ? null : tokenInput.process(null);
	}
	
	public void setToken(String token) {
		if (tokenOutput != null) tokenOutput.process(token);
	}

	public void setName(K name) {
		this.name = name;
	}

	public void setProtocol(IProcessor<ClientMsg, ClientMsg> protocol) {
		this.protocol = protocol;
	}

	@Override
	public R process(ClientMsg request) {
		if(request == null || request.name == null) return null;
		if (tokenInput != null) request.token = tokenInput.process(null);
		ClientMsg response = protocol.process(request);
		if (response == null) return null;
		else {
			if (tokenOutput != null) tokenOutput.process(response.token);
			if(response.error != null) throw new PlatformException(response.error);
			else return (R) response.content;
		}
	}
	
	public R perform(V parameters){
		return perform(name, null, parameters);
	}
	
	public R perform(K name, V parameters){
		return perform(name, null, parameters);
	}
	
	public R perform(K name, String id, V parameters){
		ClientMsg msg = new ClientMsg();
		if(name == null) msg.name = this.name.toString();
		else msg.name = name.toString();
		msg.id  = id;
		msg.content = parameters;
		return process(msg);
	}	
	
	public static class DefaultToken implements IProcessor<Void, String>{
		protected volatile ThreadLocal<String> token = new ThreadLocal<String>();
		@Override
		public String process(Void instance) {
			return token.get();
		}
		
		public class Output implements IProcessor<String, Void>{
			@Override
			public Void process(String instance) {
				token.set(instance);
				return null;
			}
		}
	}
}
