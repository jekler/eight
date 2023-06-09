package net.yeeyaa.eight.service.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.service.ServiceMsg;


public class ContentProcessor implements IProcessor<ServiceMsg, Object>{
	protected Type type;
	public enum Type{token, name, id, content, error, ext}
	
	public void setType(Type type) {
		this.type = type;
	}

	@Override
	public Object process(ServiceMsg msg) {
		if(msg != null && type != null) switch (type){
			case token : return msg.token;
			case name : return msg.name;
			case id : return msg.id;
			case content : return msg.content;
			case error : return msg.error;
			default : return msg.ext; 
		} else return null;
	}
}
