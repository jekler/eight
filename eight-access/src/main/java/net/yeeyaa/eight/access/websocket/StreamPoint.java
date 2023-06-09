package net.yeeyaa.eight.access.websocket;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.Type;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.WebSocketSession;


public class StreamPoint implements IBiProcessor<WebSocketSession, BinaryMessage, Void>, Runnable {
	protected static final Pattern url = Pattern.compile("(([^\\?&=]+)=([^&=]+))");
	protected final Logger log;
	protected IProcessor<IBiProcessor<Object, Object, Object>, Object> stream;
	protected IResource<String, byte[]> resource;
	protected IProcessor<WebSocketSession, WebSocketSession> preProcessor;
	protected ITriProcessor<WebSocketSession, Integer, Byte, Boolean> filter;
	protected String charset = Charset.defaultCharset().name();
	protected ExecutorService executor;
	protected IProcessor<Object, Object> destroy;
	protected int buffer;
	protected int size;
	protected long timeout;
	protected boolean listener;
	protected final Map<String, Entry<Entry<Entry<List<byte[]>, Map<String, List<String>>>, Integer>, Long>> cache = new ConcurrentHashMap<String, Entry<Entry<Entry<List<byte[]>, Map<String, List<String>>>, Integer>, Long>>();
	
	public StreamPoint() {
		this.log = LoggerFactory.getLogger(StreamPoint.class);
	}

	public StreamPoint(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(StreamPoint.class) : log;
	}
	
	public void setDestroy(IProcessor<Object, Object> destroy) {
		this.destroy = destroy;
	}

	public void setFilter(ITriProcessor<WebSocketSession, Integer, Byte, Boolean> filter) {
		this.filter = filter;
	}

	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}
	
	public void setPreProcessor(IProcessor<WebSocketSession, WebSocketSession> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setCharset(String charset) {
		if (charset != null) this.charset = charset;
	}

	public void setStream(IProcessor<IBiProcessor<Object, Object, Object>, Object> stream) {
		this.stream = stream;
	}

	public void setResource(IResource<String, byte[]> resource) {
		this.resource = resource;
	}

	public void setSize(Integer size) {
		if (size != null && size > 5) this.size = size;
	}

	public void setBuffer(Integer buffer) {
		if (buffer != null && buffer > 0) this.buffer = buffer;
	}

	public void setTimeout(Long timeout) {
		if (timeout != null && timeout > 0) this.timeout = timeout;
	}

	public void setListener(Boolean listener) {
		if (listener != null) this.listener = listener;
	}

	@PreDestroy
	public void destroy() {
		if (destroy != null) destroy.process(executor);
	}
	
	protected class Wrapper {
		protected IProcessor<OutputStream, Object> processor;

		public Wrapper(IProcessor<OutputStream, Object> processor) {
			this.processor = processor;
		}
	}
	
	protected class Para implements IBiProcessor<Object, Object, Object> {
		protected InputStream input;
		protected Map<String, List<String>> map;
		
		public Para(InputStream input, Map<String, List<String>> map) {
			this.input = input;
			this.map = map;
		}

		@Override
		public Object perform(Object instance, Object para) {
			if (instance == null) return input;
			else if (instance instanceof IProcessor) return new Wrapper((IProcessor<OutputStream, Object>) instance);
			else if (para instanceof Integer) {
				Integer index = ((Integer) para) < 0 ? 0 : ((Integer) para);
				List<String> list = map.get(instance.toString());
				if (list == null || index >= list.size()) return null;
				else return list.get(index);
			} else return map.get(instance.toString());
		}
	}
	
	protected void write(WebSocketSession session, Integer id, byte[] content) throws IOException {
		byte[] ids = toBytes(id);
		if (size < 6 || content.length + 5 <= size) {
			byte[] ret = new byte[content.length + 5];
			System.arraycopy(ids, 0, ret, 0, 4);
			ret[4] = 64;
			System.arraycopy(content, 0, ret, 5, content.length);
			session.sendMessage(new BinaryMessage(ret));
		} else {
			int count = content.length / (size - 5);
			byte[] ret = new byte[size];
			System.arraycopy(ids, 0, ret, 0, 4);
			for (int i = 0; i < count; i ++) {
				System.arraycopy(content, (size - 5) * i, ret, 5, size - 5);
				session.sendMessage(new BinaryMessage(ret));
			}
			ret = new byte[content.length - count * (size - 5) + 5];
			System.arraycopy(ids, 0, ret, 0, 4);
			ret[4] = 64;
			System.arraycopy(content, (size - 5) * count, ret, 5, content.length - count * (size - 5));
			session.sendMessage(new BinaryMessage(ret));
		}
	}
	
	protected byte[] toBytes(int i) {
		byte[] result = new byte[4];
		result[0] = (byte) (i >> 24);
		result[1] = (byte) (i >> 16);
		result[2] = (byte) (i >> 8);
		result[3] = (byte) (i);
		return result;
	}
	
	protected void error(WebSocketSession session, Integer id, String error, Integer code, String msg) throws IOException {
		byte[] e = error == null ? new byte[0] : error.getBytes(charset);
		byte[] m = msg == null ? new byte[0] : msg.getBytes(charset);
		byte[] ret = new byte[e.length + m.length + 13];
		System.arraycopy(toBytes(id), 0, ret, 0, 4);
		ret[4] = (byte)224;
		System.arraycopy(toBytes(e.length), 0, ret, 5, 4);
		System.arraycopy(e, 0, ret, 9, e.length);
		System.arraycopy(toBytes(code), 0, ret, 9 + e.length, 4);
		System.arraycopy(m, 0, ret, 13 + e.length, m.length);	
		session.sendMessage(new BinaryMessage(ret));
	}
	
	@Override
	public Void perform(WebSocketSession session, BinaryMessage msg) {
		int id = 0;
		try {
			if (preProcessor != null) session = preProcessor.process(session);
			int length = msg.getPayloadLength();
			if (length > 5) {
				ByteBuffer buf = msg.getPayload();
				id = buf.getInt();
				byte flag = buf.get();
				if (filter == null || filter.operate(session, id, flag)) if ((flag & 0x80) == 0x80) {
					if ((flag & 0x20) == 0x20) {
						int paralength = buf.getInt();
						HashMap<String, List<String>> map = new HashMap<String, List<String>>();
						if (paralength > 0 && paralength < length - 8) {
							byte[] para = new byte[paralength];
							for (int i = 0; i < paralength; i ++) para[i] = buf.get();
					    	Matcher m = url.matcher(new String(para, charset));
							while (m.find()) {
								String key = m.group(2);
								List<String> value = map.get(key);
								if (value == null) {
									value = new LinkedList<String>();
									map.put(key, value);
								}
								value.add(m.group(3));
							}
						}
					}
					byte[] data = new byte[buf.remaining()];
					if (data.length > 0) for (int i = 0; i < data.length; i++) data[i] = buf.get();
					if ((flag & 0x40) == 0x40) {
	
					}
					
				} else {
					
				}
			}
		} catch (Exception e) {
			log.error("StreamPoint: process error.", e);
			try {
				if (e instanceof PlatformException) {
					Type type = ((PlatformException) e).getType();
					error(session, id, type.getCate(), type.getCode(), type.getMessage());
				} else error(session, id, "WebSocketError", 0, "ERROR_MSG_FORMAT");
			} catch (IOException ex) {
				log.error("StreamPoint: process error.", e);
			}
		}
		return null;
	}

	@Override
	public void run() {
		Long time = System.currentTimeMillis() - timeout;
		Iterator<Entry<String, Entry<Entry<Entry<List<byte[]>, Map<String, List<String>>>, Integer>, Long>>> itr = cache.entrySet().iterator();
		while (itr.hasNext()) try {
			Entry<String, Entry<Entry<Entry<List<byte[]>, Map<String, List<String>>>, Integer>, Long>> entry = itr.next();
			if (entry.getValue().getValue() < time) {
				resource.discard(entry.getKey());
				itr.remove();
			}
		} catch (Exception e) {
			log.error("StreamPoint: recycle timeout stream error.", e);
		}
	}
}
