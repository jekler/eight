package net.yeeyaa.eight.data.util;

import net.yeeyaa.eight.IProcessor;

import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.SerializationException;


public class ProcessorSerializer<T> implements RedisSerializer<T> {
	protected IProcessor<T, byte[]> output;
	protected IProcessor<byte[], T> input;
	
	public void setOutput(IProcessor<T, byte[]> output) {
		this.output = output;
	}

	public void setInput(IProcessor<byte[], T> input) {
		this.input = input;
	}

	@Override
	public byte[] serialize(T t) throws SerializationException {
		if (output == null) return (byte[]) t;
		else return output.process(t);
	}

	@Override
	public T deserialize(byte[] bytes) throws SerializationException {
		if (input == null) return (T) bytes;
		else return input.process(bytes);
	}
}
