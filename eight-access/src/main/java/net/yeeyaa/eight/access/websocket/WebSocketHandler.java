package net.yeeyaa.eight.access.websocket;

import java.util.List;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.PongMessage;
import org.springframework.web.socket.SubProtocolCapable;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.AbstractWebSocketHandler;


public class WebSocketHandler extends AbstractWebSocketHandler implements SubProtocolCapable {
	protected Boolean partial = false;
	protected List<String> protocols;
	protected IProcessor<WebSocketSession, Void> establish;
	protected IBiProcessor<WebSocketSession, CloseStatus, Void> close;
	protected IBiProcessor<WebSocketSession, Throwable, Void> error;	
	protected IBiProcessor<WebSocketSession, TextMessage, Void> text;
	protected IBiProcessor<WebSocketSession, BinaryMessage, Void> binary;
	protected IBiProcessor<WebSocketSession, PongMessage, Void> pong;
	protected IBiProcessor<WebSocketSession, WebSocketMessage<?>, Void> message;
	
    public void setProtocols(List<String> protocols) {
		this.protocols = protocols;
	}

	public void setPartial(Boolean partial) {
		if (partial != null) this.partial = partial;
	}

	@Override
	public void afterConnectionEstablished(WebSocketSession session) throws Exception {
		if (establish != null) establish.process(session);
	}

	@Override
	public void afterConnectionClosed(WebSocketSession session, CloseStatus status) throws Exception {
		if (close != null) close.perform(session, status);
	}

	@Override
	public void handleTransportError(WebSocketSession session, Throwable exception) throws Exception {
		if (error != null) error.perform(session, exception);
	}

	@Override
	public boolean supportsPartialMessages() {
		return partial;
	}

	@Override
	public void handleMessage(WebSocketSession session, WebSocketMessage<?> message) throws Exception {
		if (this.message == null) super.handleMessage(session, message);
		else this.message.perform(session, message);
		
	}

	@Override
	protected void handleTextMessage(WebSocketSession session, TextMessage message) {
    	if (text != null) text.perform(session, message);
    }

	@Override
	protected void handleBinaryMessage(WebSocketSession session, BinaryMessage message) throws Exception {
		if (binary != null) binary.perform(session, message);
	}

	@Override
	protected void handlePongMessage(WebSocketSession session, PongMessage message) throws Exception {
		if (pong != null) pong.perform(session, message);
	}

	@Override
	public List<String> getSubProtocols() {
		return protocols;
	}
}
