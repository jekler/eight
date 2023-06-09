package net.yeeyaa.eight.access.jms;

import java.io.Serializable;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.jms.support.converter.MessageConversionException;
import org.springframework.jms.support.converter.MessageConverter;
import org.springframework.jms.support.converter.SimpleMessageConverter;


public class JmsProcessor implements IProcessor<Object, Object> {
	protected final Logger log;
	protected MessageConverter convertor = new SimpleMessageConverter() {
		@Override
		public Message toMessage(Object object, Session session) throws JMSException, MessageConversionException {
			if (!(object instanceof Serializable) && !(object instanceof Message)) object = "";
			return super.toMessage(object, session);
		}
	};
	protected JmsTemplate template;
	protected Object destination;
	protected Boolean type;
	
	public JmsProcessor() {
		this.log = LoggerFactory.getLogger(JmsProcessor.class);
	}

	public JmsProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(JmsProcessor.class) : log;
	}
	
	public void setDestination(Object destination) {
		this.destination = destination;
	}

	public void setType(Boolean type) {
		this.type = type;
	}

	public void setTemplate(JmsTemplate template) {
		this.template = template;
	}

	public void setConvertor(MessageConverter convertor) {
		if(convertor != null) this.convertor = convertor;
	}
	
	@Override
	public Object process(final Object in) {
		try {
			if (Boolean.TRUE.equals(type) && in instanceof Object[] && ((Object[])in).length > 0) {
				MessageCreator mc = new MessageCreator() {
			        public Message createMessage(Session session) throws JMSException {
			        	Object value = ((Object[])in)[0];
			        	return convertor.toMessage(value, session);
				    }
				};
				if(((Object[])in).length < 2 || ((Object[])in)[1] == null) template.send(mc); 
				else if (((Object[])in)[1] instanceof Destination) template.send((Destination)((Object[])in)[1], mc); 
				else template.send(((Object[])in)[1].toString(), mc);
			} else {
				MessageCreator mc = new MessageCreator() {
			        public Message createMessage(Session session) throws JMSException {
			        	return convertor.toMessage(in, session);
				    }
				};
				if (destination == null) template.send(mc); 
				else if (destination instanceof Destination) template.send((Destination)destination, mc); 
				else template.send(destination.toString(), mc);
			}
		} catch (Exception e) {
			log.error("jms service error:", e);
		}
		return in;
	}
}
