package net.yeeyaa.eight.access.websocket;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.WebSocketSession;


public class BinaryPoint implements IBiProcessor<WebSocketSession, BinaryMessage, Void> {
	protected final Logger log;
	protected IProcessor<byte[], byte[]> next;
	protected IProcessor<WebSocketSession, WebSocketSession> preProcessor;
	protected Boolean listener = false;
	
	public BinaryPoint() {
		this.log = LoggerFactory.getLogger(BinaryPoint.class);
	}

	public BinaryPoint(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BinaryPoint.class) : log;
	}
	
	public void setNext(IProcessor<byte[], byte[]> next) {
		this.next = next;
	}

	public void setPreProcessor(IProcessor<WebSocketSession, WebSocketSession> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setListener(Boolean listener) {
		if (listener != null) this.listener = listener;
	}

	@Override
	public Void perform(WebSocketSession key, BinaryMessage content) {
		try {
			if (preProcessor != null) key = preProcessor.process(key);
			byte[] ret = next.process(content.getPayload().array());
			if (ret != null && !listener) key.sendMessage(new BinaryMessage(ret));
		} catch (Exception e) {
			log.error("BinaryPoint: process error.", e);
		}
		return null;
	}
}
