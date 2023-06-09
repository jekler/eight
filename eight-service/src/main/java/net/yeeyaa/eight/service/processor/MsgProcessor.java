package net.yeeyaa.eight.service.processor;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException.Type;
import net.yeeyaa.eight.service.ServiceMsg;


public class MsgProcessor implements IProcessor<ServiceMsg, ServiceMsg>{
	protected IProcessor<String, String> token;
	protected IProcessor<String, String> name;
	protected IProcessor<String, String> id;
	protected IProcessor<Object, Object> content;
	protected IProcessor<Type, Type> error;
	protected IProcessor<Map<String, Object>, Map<String, Object>> ext;
	
	public void setToken(IProcessor<String, String> token) {
		this.token = token;
	}

	public void setName(IProcessor<String, String> name) {
		this.name = name;
	}

	public void setId(IProcessor<String, String> id) {
		this.id = id;
	}

	public void setContent(IProcessor<Object, Object> content) {
		this.content = content;
	}

	public void setError(IProcessor<Type, Type> error) {
		this.error = error;
	}

	public void setExt(IProcessor<Map<String, Object>, Map<String, Object>> ext) {
		this.ext = ext;
	}

	@Override
	public ServiceMsg process(ServiceMsg msg) {
		if(msg != null){
			ServiceMsg newMsg = new ServiceMsg();
			if(token != null) newMsg.token = token.process(msg.token);
			else newMsg.token = msg.token;
			if(name != null) newMsg.name = name.process(msg.name);
			else newMsg.name = msg.name;
			if(id != null) newMsg.id = id.process(msg.id);
			else newMsg.id = msg.id;
			if(content != null) newMsg.content = content.process(msg.content);
			else newMsg.content = msg.content;
			if(error != null) newMsg.error = error.process(msg.error);
			else newMsg.error = msg.error;
			if(ext != null) newMsg.ext = ext.process(msg.ext);
			else newMsg.ext = msg.ext;
			return newMsg;
		} else return null;
	}
}
