package net.yeeyaa.eight.service.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.service.ServiceMsg;


public class StreamProcessor implements IProcessor<IBiProcessor<String, Integer, String>, ServiceMsg>{
	@Override
	public ServiceMsg process(IBiProcessor<String, Integer, String> msg) {
		if(msg != null) {
			ServiceMsg m = new ServiceMsg();
			m.token = msg.perform("token", 0);
			m.id = msg.perform("id", 0);
			m.name = msg.perform("name", 0);
			m.content = msg;
			return m;
		} else return null;
	}
}
