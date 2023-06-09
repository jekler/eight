package net.yeeyaa.eight.access.jms;

import javax.jms.Message;
import javax.jms.MessageListener;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.support.converter.MessageConverter;
import org.springframework.jms.support.converter.SimpleMessageConverter;


public class JmsListener implements MessageListener{
	protected final Logger log;
	protected MessageConverter convertor = new SimpleMessageConverter();
	protected IProcessor<Object, Void> next;
	protected IProcessor<Message, Message> preProcessor;
	
	public JmsListener() {
		this.log = LoggerFactory.getLogger(JmsListener.class);
	}

	public JmsListener(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(JmsListener.class) : log;
	}
	
	public void setPreProcessor(IProcessor<Message, Message> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setConvertor(MessageConverter convertor) {
		if(convertor != null) this.convertor = convertor;
	}
	
	public void setNext(IProcessor<Object, Void> next) {
		this.next = next;
	}

	@Override
	public void onMessage(Message message) {
	    try{
	    	if(preProcessor != null) message = preProcessor.process(message);
	    	next.process(convertor.fromMessage(message));
	    }catch(Exception e){
			log.error("jms service error:", e);
        }
	}
}
