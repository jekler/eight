package net.yeeyaa.eight.access.websocket;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;

import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.SubProtocolCapable;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.WebSocketMessage;
import org.springframework.web.socket.WebSocketSession;

public class WebSocketDispatcher implements WebSocketHandler, SubProtocolCapable{
	protected WebSocketHandler defaultHandler;
	protected Map<String, WebSocketHandler> handlers;
	protected Boolean partial = true;
	protected volatile List<String> protocols;
	protected Boolean ignore = false;
	
	public void setPartial(Boolean partial) {
		if (partial != null) this.partial = partial;
	}
    
	public void setIgnore(Boolean ignore) {
		if (ignore != null) this.ignore = ignore;
	}

	public void setDefaultHandler(WebSocketHandler defaultHandler) {
		this.defaultHandler = defaultHandler;
	}

	public void setHandlers(Map<String, WebSocketHandler> handlers) {
		this.handlers = handlers;
	}

	protected WebSocketHandler select(WebSocketSession session) {
		WebSocketHandler handler = handlers.get(session.getAcceptedProtocol());
		if (handler != null) return handler;
		else return defaultHandler;
	}
	
	@Override
	public List<String> getSubProtocols() {
		if (protocols == null) {
			HashSet<String> set = new HashSet<String>();
			if (defaultHandler != null && defaultHandler instanceof SubProtocolCapable) {
				List<String> ls = ((SubProtocolCapable)defaultHandler).getSubProtocols();
				if (ls != null && ls.size() > 0) set.addAll(ls);
			}
			if (handlers != null && handlers.size() > 0) set.addAll(handlers.keySet());
			List<String> tmp = new LinkedList<String>();
			for (String proto : set) if (proto != null) tmp.add(proto);
			protocols = tmp;
		}
		return protocols;
	}

	@Override
	public void afterConnectionEstablished(WebSocketSession session) throws Exception {
		WebSocketHandler handler = select(session);
		if (handler != null) {
			handler.afterConnectionEstablished(session);
			return;
		}
		if (!ignore) throw new ServletException("Cannot find handler");
	}

	@Override
	public void handleMessage(WebSocketSession session,	WebSocketMessage<?> message) throws Exception {
		WebSocketHandler handler = select(session);
		if (handler != null) {
			handler.handleMessage(session, message);
			return;
		}
		if (!ignore) throw new ServletException("Cannot find handler");
	}

	@Override
	public void handleTransportError(WebSocketSession session, Throwable exception) throws Exception {
		WebSocketHandler handler = select(session);
		if (handler != null) {
			handler.handleTransportError(session, exception);
			return;
		}
		if (!ignore) throw new ServletException("Cannot find handler");
	}

	@Override
	public void afterConnectionClosed(WebSocketSession session,	CloseStatus closeStatus) throws Exception {
		WebSocketHandler handler = select(session);
		if (handler != null) {
			handler.afterConnectionClosed(session, closeStatus);
			return;
		}
		if (!ignore) throw new ServletException("Cannot find handler");
	}

	@Override
	public boolean supportsPartialMessages() {
		return partial;
	}
}
