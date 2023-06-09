package net.yeeyaa.eight.access.websocket;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;


public class TextPoint implements IBiProcessor<WebSocketSession, TextMessage, Void> {
	protected final Logger log;
	protected IProcessor<String, String> next;
	protected IProcessor<WebSocketSession, WebSocketSession> preProcessor;
	protected Boolean listener = false;
	
	public TextPoint() {
		this.log = LoggerFactory.getLogger(TextPoint.class);
	}

	public TextPoint(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(TextPoint.class) : log;
	}
	
	public void setNext(IProcessor<String, String> next) {
		this.next = next;
	}

	public void setPreProcessor(IProcessor<WebSocketSession, WebSocketSession> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setListener(Boolean listener) {
		if (listener != null) this.listener = listener;
	}

	@Override
	public Void perform(WebSocketSession key, TextMessage content) {
		try {
			if (preProcessor != null) key = preProcessor.process(key);
			String ret = next.process(content.getPayload());
			if (ret != null && !listener) key.sendMessage(new TextMessage(ret));
		} catch (Exception e) {
			log.error("TextPoint: process error.", e);
		}
		return null;
	}
}
