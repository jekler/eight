package net.yeeyaa.eight.access.redis;

import net.yeeyaa.eight.IProcessor;

import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.core.RedisTemplate;


public class RedisPoint implements MessageListener{
	protected RedisTemplate<String, Object> template;
	protected String channel;
	protected Boolean type;
	protected IProcessor<Object, Object> next;
	protected IProcessor<Message, Message> preProcessor;
	protected Boolean nullable = true;
	
	public void setNullable(Boolean nullable) {
		if (nullable != null) this.nullable = nullable;
	}

	public void setPreProcessor(IProcessor<Message, Message> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setTemplate(RedisTemplate<String, Object> template) {
		this.template = template;
	}

	public void setChannel(String channel) {
		this.channel = channel;
	}

	public void setType(Boolean type) {
		this.type = type;
	}

	public void setNext(IProcessor<Object, Object> next) {
		this.next = next;
	}

	@Override
	public void onMessage(Message message, byte[] pattern) {
    	if(preProcessor != null) message = preProcessor.process(message);
    	Object resp;
		if (type == null) resp = next.process(template.getValueSerializer() == null || message.getBody() == null ? message.getBody() : template.getValueSerializer().deserialize(message.getBody()));
		else if (type) resp = next.process(new Object[]{template.getValueSerializer() == null || message.getBody() == null ? message.getBody() : template.getValueSerializer().deserialize(message.getBody()), 
				template.getKeySerializer() == null || message.getChannel() == null ? message.getChannel() : template.getKeySerializer().deserialize(message.getChannel()), 
				template.getKeySerializer() == null || pattern == null ? pattern : template.getKeySerializer().deserialize(pattern)});
		else resp = next.process(template.getKeySerializer() == null || message.getChannel() == null ? message.getChannel() : template.getKeySerializer().deserialize(message.getChannel()));
		if (resp != null || nullable) if (Boolean.TRUE.equals(type) && resp instanceof Object[] && ((Object[])resp).length > 1 && ((Object[])resp)[1] instanceof String) template.convertAndSend((String)((Object[])resp)[1], ((Object[])resp)[0]); 
		else template.convertAndSend(channel, resp);
	}	
}
