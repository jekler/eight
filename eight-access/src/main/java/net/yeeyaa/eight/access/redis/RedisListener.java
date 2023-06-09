package net.yeeyaa.eight.access.redis;

import net.yeeyaa.eight.IProcessor;

import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.serializer.RedisSerializer;


public class RedisListener implements MessageListener{
	protected RedisSerializer<?> valueSerializer;
	protected RedisSerializer<?> keySerializer;
	protected IProcessor<Object, Void> next;
	protected IProcessor<Message, Message> preProcessor;
	protected Boolean type; 
	
	public void setPreProcessor(IProcessor<Message, Message> preProcessor) {
		this.preProcessor = preProcessor;
	}
	
	public void setValueSerializer(RedisSerializer<?> valueSerializer) {
		this.valueSerializer = valueSerializer;
	}

	public void setKeySerializer(RedisSerializer<?> keySerializer) {
		this.keySerializer = keySerializer;
	}

	public void setNext(IProcessor<Object, Void> next) {
		this.next = next;
	}

	@Override
	public void onMessage(Message message, byte[] pattern) {
    	if(preProcessor != null) message = preProcessor.process(message);
		if (type == null) next.process(valueSerializer == null || message.getBody() == null ? message.getBody() : valueSerializer.deserialize(message.getBody()));
		else if (type) next.process(new Object[]{valueSerializer == null || message.getBody() == null ? message.getBody() : valueSerializer.deserialize(message.getBody()), 
			keySerializer == null || message.getChannel() == null ? message.getChannel() : keySerializer.deserialize(message.getChannel()), keySerializer == null || pattern == null ? pattern : keySerializer.deserialize(pattern)});
		else next.process(keySerializer == null || message.getChannel() == null ? message.getChannel() : keySerializer.deserialize(message.getChannel()));
	}
}
