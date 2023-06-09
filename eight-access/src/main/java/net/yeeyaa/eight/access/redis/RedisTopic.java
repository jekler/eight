package net.yeeyaa.eight.access.redis;

import net.yeeyaa.eight.IProcessor;

import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.Topic;


public class RedisTopic implements IProcessor<Object, Topic> {
	protected Boolean type = true;
	
	public void setType(Boolean type) {
		if (type != null) this.type = type;
	}

	@Override
	public Topic process(Object instance) {
		if (instance == null) return null;
		else if (type) return new ChannelTopic(instance.toString());
		else return new PatternTopic(instance.toString());
	}
}
