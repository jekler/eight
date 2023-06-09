package net.yeeyaa.eight.access.websocket;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import net.yeeyaa.eight.IProcessor;

import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.PongMessage;
import org.springframework.web.socket.SubProtocolCapable;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.AbstractWebSocketHandler;


public class WebSocketPoint extends AbstractWebSocketHandler implements SubProtocolCapable {
	protected String context = "/";
	protected IProcessor<byte[], byte[]> next;
	protected IProcessor<WebSocketSession, WebSocketSession> preProcessor;
	protected Boolean compress = false;	
	protected Boolean partial = true;
	protected Boolean requireReturn;
	protected List<String> protocols = Collections.EMPTY_LIST;
	
    public void setProtocols(List<String> protocols) {
		if (protocols != null) this.protocols = protocols;
	}

	public void setRequireReturn(Boolean requireReturn) {
		this.requireReturn = requireReturn;
	}

	public void setContext(String context) {
		this.context = context;
	}

	public void setNext(IProcessor<byte[], byte[]> next) {
		this.next = next;
	}

	public void setPreProcessor(IProcessor<WebSocketSession, WebSocketSession> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setCompress(Boolean compress) {
		if (compress != null) this.compress = compress;
	}

	public void setPartial(Boolean partial) {
		if (partial != null) this.partial = partial;
	}

	@Override
	public void afterConnectionEstablished(WebSocketSession session)
			throws Exception {

		super.afterConnectionEstablished(session);
	}

	@Override
	public void afterConnectionClosed(WebSocketSession session,
			CloseStatus status) throws Exception {

		super.afterConnectionClosed(session, status);
	}

	@Override
	public void handleTransportError(WebSocketSession session,
			Throwable exception) throws Exception {

		super.handleTransportError(session, exception);
	}

	@Override
	public boolean supportsPartialMessages() {
		return partial;
	}

	@Override
	protected void handleTextMessage(WebSocketSession session, TextMessage message) {
    	try {

    		Thread t = Thread.currentThread();
        	System.out.println(Thread.currentThread());       		
        	System.out.println(message.getPayload());   		
			session.sendMessage(message);
		} catch (IOException e) {

			e.printStackTrace();
		}
    }

	@Override
	protected void handleBinaryMessage(WebSocketSession session, BinaryMessage message) throws Exception {
		System.out.println(message.getPayload().array());

		super.handleBinaryMessage(session, message);
	}

	@Override
	protected void handlePongMessage(WebSocketSession session, PongMessage message) throws Exception {
		System.out.println(message.getPayload());

		super.handlePongMessage(session, message);
	}

	@Override
	public List<String> getSubProtocols() {
		return protocols;
	}
}
