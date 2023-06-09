package net.yeeyaa.eight.access.redis;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import net.yeeyaa.eight.IProcessor;

import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;


public class RedisContainer implements IProcessor<Object, Object> {
	protected MessageListener listener;
	protected RedisMessageListenerContainer container;
	protected Boolean type;
	
	public void setListener(MessageListener listener) {
		this.listener = listener;
	}

	public void setContainer(RedisMessageListenerContainer container) {
		this.container = container;
	}

	public void setType(Boolean type) {
		this.type = type;
	}

	@Override
	public Object process(Object instance) {
		if (type == null) {
			if (instance instanceof Collection) container.addMessageListener(listener, (Collection<? extends Topic>) instance);
			else if (instance instanceof Topic) container.addMessageListener(listener, (Topic) instance);
			else if (instance != null) container.addMessageListener(listener, new ChannelTopic(instance.toString()));
		} else if (type) {
			HashMap<MessageListener, Collection<? extends Topic>> map = new HashMap<MessageListener, Collection<? extends Topic>>(2);
			if (instance instanceof Collection) map.put(listener, (Collection<? extends Topic>) instance);
			else {
				ArrayList<Topic> topic = new ArrayList<Topic>(1);
				if (instance instanceof Topic) topic.add((Topic)instance);
				else if (instance != null) topic.add(new ChannelTopic(instance.toString()));
				map.put(listener, topic);
			}
			container.setMessageListeners(map);
		} else if (instance instanceof Collection) container.removeMessageListener(listener, (Collection<? extends Topic>) instance);
		else if (instance instanceof Topic) container.removeMessageListener(listener, (Topic) instance);
		else if (instance == null) container.removeMessageListener(listener);
		else container.removeMessageListener(listener, new ChannelTopic(instance.toString()));
		return instance;
	}
}
