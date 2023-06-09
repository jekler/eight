package net.yeeyaa.eight.client.processor;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.client.data.ClientMsg;

import org.eclipse.persistence.dynamic.DynamicEntity;


public class DynamicMsgProcessor implements IProcessor<DynamicEntity, ClientMsg>{
	protected IProcessor<Object, String> token;
	protected IProcessor<Object, String> name;
	protected IProcessor<Object, String> id;
	protected IProcessor<Object, Object> content;
	protected IProcessor<Object, DefaultError> error;
	protected IProcessor<Object, Map<String, Object>> ext;
	
	public void setToken(IProcessor<Object, String> token) {
		this.token = token;
	}

	public void setName(IProcessor<Object, String> name) {
		this.name = name;
	}

	public void setId(IProcessor<Object, String> id) {
		this.id = id;
	}

	public void setContent(IProcessor<Object, Object> content) {
		this.content = content;
	}

	public void setError(IProcessor<Object, DefaultError> error) {
		this.error = error;
	}

	public void setExt(IProcessor<Object, Map<String, Object>> ext) {
		this.ext = ext;
	}
	
	@Override
	public ClientMsg process(DynamicEntity msg) {
		if(msg != null){
			ClientMsg newMsg = new ClientMsg();
			newMsg.token = this.token == null ? msg.<String>get("token") : this.token.process(msg.get("token"));
			newMsg.name = this.name == null ? msg.<String>get("name") : this.name.process(msg.get("name"));
			newMsg.id = this.id == null ? msg.<String>get("id") : this.id.process(msg.get("id"));			
			newMsg.content = this.content == null ? msg.get("content") : this.content.process(msg.get("content"));
			newMsg.error = this.error == null ? msg.<DefaultError>get("error") : this.error.process(msg.get("error"));
			newMsg.ext = this.ext == null ? msg.<Map<String, Object>>get("ext") : this.ext.process(msg.get("ext"));
			return newMsg;
		} else return null;
	}
}
