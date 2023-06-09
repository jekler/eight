package net.yeeyaa.eight.service.processor;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException.Type;
import net.yeeyaa.eight.service.ServiceMsg;

import org.eclipse.persistence.dynamic.DynamicEntity;


public class MsgDynamicProcessor implements IProcessor<ServiceMsg, DynamicEntity>{
	protected IProcessor<Object, DynamicEntity> factory;
	protected Object key;
	protected IProcessor<String, Object> token;
	protected IProcessor<String, Object> name;
	protected IProcessor<String, Object> id;
	protected IProcessor<Object, Object> content;
	protected IProcessor<Type, Object> error;
	protected IProcessor<Map<String, Object>, Object> ext;
	
	public void setToken(IProcessor<String, Object> token) {
		this.token = token;
	}

	public void setName(IProcessor<String, Object> name) {
		this.name = name;
	}

	public void setId(IProcessor<String, Object> id) {
		this.id = id;
	}

	public void setContent(IProcessor<Object, Object> content) {
		this.content = content;
	}

	public void setError(IProcessor<Type, Object> error) {
		this.error = error;
	}

	public void setExt(IProcessor<Map<String, Object>, Object> ext) {
		this.ext = ext;
	}
	
	public void setFactory(IProcessor<Object, DynamicEntity> factory) {
		this.factory = factory;
	}

	public void setKey(Object key) {
		this.key = key;
	}

	@Override
	public DynamicEntity process(ServiceMsg msg) {
		if(msg != null){
			DynamicEntity newMsg = factory.process(key);
			Object token = this.token == null ? msg.token : this.token.process(msg.token);
			if (token != null) newMsg.set("token", token);
			Object name = this.name == null ? msg.name : this.name.process(msg.name);
			if (name != null) newMsg.set("name", name);
			Object id = this.id == null ? msg.id : this.id.process(msg.id);
			if (id != null) newMsg.set("id", id);		
			Object content = this.content == null ? msg.content : this.content.process(msg.content);
			if (content != null) newMsg.set("content", content);
			Object error = this.error == null ? msg.error : this.error.process(msg.error);
			if (error != null) newMsg.set("error", error);
			Object ext = this.ext == null ? msg.ext : this.ext.process(msg.ext);
			if (ext != null) newMsg.set("ext", msg);
			return newMsg;
		} else return null;
	}
}
