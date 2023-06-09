package net.yeeyaa.eight.access.redis;

import net.yeeyaa.eight.IProcessor;

import org.springframework.data.redis.core.RedisTemplate;


public class RedisProcessor implements IProcessor<Object, Object> {
	protected RedisTemplate<String, Object> template;
	protected String channel;
	protected Boolean type;
	
	public void setChannel(String channel) {
		this.channel = channel;
	}

	public void setType(Boolean type) {
		this.type = type;
	}

	public void setTemplate(RedisTemplate<String, Object> template) {
		this.template = template;
	}

	@Override
	public Object process(Object message) {
		if (Boolean.TRUE.equals(type) && message instanceof Object[] && ((Object[])message).length > 1 && ((Object[])message)[1] instanceof String) template.convertAndSend((String)((Object[])message)[1], ((Object[])message)[0]); 
		else template.convertAndSend(channel, message);
		return message;
	}
}
