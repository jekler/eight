package net.yeeyaa.eight.client.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.client.data.ClientMsg;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ContentProcessor implements IProcessor<ClientMsg, Object>{
	protected enum Type{token, name, id, content, error, ext}
	protected final Logger log;
	protected Type type;
	
	public ContentProcessor() {
		this.log = LoggerFactory.getLogger(ContentProcessor.class);
	}

	public ContentProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ContentProcessor.class) : log;
	}
	
	public void setType(String type) {
		try {
			this.type = Type.valueOf(type);
		} catch (Exception e) {
			log.error("ContentProcessor: init type fail.", e);
		}
	}

	@Override
	public Object process(ClientMsg msg) {
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
