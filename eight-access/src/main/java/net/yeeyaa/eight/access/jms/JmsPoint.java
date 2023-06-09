package net.yeeyaa.eight.access.jms;

import java.io.Serializable;
import java.util.HashMap;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.jms.support.converter.MessageConversionException;
import org.springframework.jms.support.converter.MessageConverter;
import org.springframework.jms.support.converter.SimpleMessageConverter;


public class JmsPoint implements MessageListener{
	protected final Logger log;
	protected MessageConverter convertor = new SimpleMessageConverter() {
		@Override
		public Message toMessage(Object object, Session session) throws JMSException, MessageConversionException {
			if (!(object instanceof Serializable) && !(object instanceof Message)) object = "";
			return super.toMessage(object, session);
		}
	};
	protected JmsTemplate template;
	protected IProcessor<Object, Object> next;
	protected IProcessor<Message, Message> preProcessor;
	protected Boolean nullable;
	protected Object destination;
	protected Boolean type;
	
	public JmsPoint() {
		this.log = LoggerFactory.getLogger(JmsPoint.class);
	}

	public JmsPoint(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(JmsPoint.class) : log;
	}
	
	public void setDestination(Object destination) {
		this.destination = destination;
	}

	public void setType(Boolean type) {
		this.type = type;
	}
	
	public void setNullable(Boolean nullable) {
		this.nullable = nullable;
	}

	public void setPreProcessor(IProcessor<Message, Message> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setConvertor(MessageConverter convertor) {
		if(convertor != null) this.convertor = convertor;
	}

	public void setTemplate(JmsTemplate template) {
		this.template = template;
	}

	public void setNext(IProcessor<Object, Object> next) {
		this.next = next;
	}

	protected Object emptyMessage(Message message){
		if (message instanceof BytesMessage) return new byte[0];
		else if (message instanceof MapMessage) return new HashMap();
		else return "";
	}
	
	@Override
	public void onMessage(Message msg) {
	    try{
	    	final Message message = preProcessor == null ? msg : preProcessor.process(msg);
	    	final Object resp = next.process(convertor.fromMessage(message));
	    	if((resp != null || !Boolean.FALSE.equals(nullable)) && template != null) if (Boolean.TRUE.equals(type) && resp instanceof Object[] && ((Object[])resp).length > 0) {
    			MessageCreator mc = new MessageCreator() {
    		        public Message createMessage(Session session) throws JMSException {
    		        	Object value = nullable == null && ((Object[])resp)[0] == null ? emptyMessage(message) : ((Object[])resp)[0];
    		        	return convertor.toMessage(value, session);
    			    }
    			};
    			if(((Object[])resp).length < 2 || ((Object[])resp)[1] == null) template.send(mc); 
    			else if (((Object[])resp)[1] instanceof Destination) template.send((Destination)((Object[])resp)[1], mc); 
    			else template.send(((Object[])resp)[1].toString(), mc);
    		} else {
    			MessageCreator mc = new MessageCreator() {
    		        public Message createMessage(Session session) throws JMSException {
    		        	Object value =  nullable == null && resp == null ? emptyMessage(message) : resp;
    		        	return convertor.toMessage(value, session);
    			    }
    			};
    			if (destination == null) template.send(mc); 
    			else if (destination instanceof Destination) template.send((Destination)destination, mc); 
    			else template.send(destination.toString(), mc);
    		}
	    }catch(Exception e){
			log.error("jms service error:", e);
        }
	}
}
