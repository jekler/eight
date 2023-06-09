package net.yeeyaa.eight.common.processor;

import java.io.ByteArrayOutputStream;
import java.util.concurrent.LinkedBlockingQueue;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.pool.KryoFactory;
import com.esotericsoftware.kryo.pool.KryoPool;


public class KryoProcessor<T> implements IProcessor<T, byte[]>, IBiProcessor<T, Integer, byte[]> {
	protected final Logger log;
	protected static final byte[] EMPTY = new byte[0];
	protected static final int DEFAULT_BUFFER = 102400;
	protected final KryoPool pool;
	
	public KryoProcessor() {
		this(null, false, 0, new Class[0]);
	}

	public KryoProcessor(boolean soft) {
		this(null, soft, 0, new Class[0]);
	}

	public KryoProcessor(Class<?>... reg) {
		this(null, false, 0, reg);
	}

	public KryoProcessor(int poolSzie) {
		this(null, false, poolSzie, new Class[0]);
	}

	public KryoProcessor(boolean soft, int poolSzie) {
		this(null, soft, poolSzie, new Class[0]);
	}

	public KryoProcessor(int poolSzie, Class<?>... reg) {
		this(null, false, poolSzie, reg);
	}

	public KryoProcessor(Logger log) {
		this(log, false, 0, new Class[0]);
	}
	
	public KryoProcessor(Logger log, boolean soft) {
		this(log, soft, 0, new Class[0]);
	}

	public KryoProcessor(Logger log, Class<?>... reg) {
		this(log, false, 0, reg);
	}

	public KryoProcessor(Logger log, int poolSzie) {
		this(log, false, poolSzie, new Class[0]);
	}

	public KryoProcessor(Logger log, boolean soft, int poolSzie) {
		this(log, soft, poolSzie, new Class[0]);
	}

	public KryoProcessor(Logger log, int poolSzie, Class<?>... reg) {
		this(log, false, poolSzie, reg);
	}
	
	public KryoProcessor(Logger log, boolean soft, int poolSize, final Class<?>... reg) {
		this.log = log == null ? LoggerFactory.getLogger(KryoProcessor.class) : log;
		KryoFactory factory = new KryoFactory() {
			@Override
			public Kryo create() {
				Kryo kryo = new Kryo();
				if (reg != null) for (Class<?> r : reg) kryo.register(r);
				return kryo;
			}
		};
		pool = soft ? poolSize > 0 ? new KryoPool.Builder(factory).queue(new LinkedBlockingQueue<Kryo>(poolSize)).softReferences().build() 
				: new KryoPool.Builder(factory).softReferences().build() 
				: poolSize > 0 ? new KryoPool.Builder(factory).queue(new LinkedBlockingQueue<Kryo>(poolSize)).build() : new KryoPool.Builder(factory).build();
	}

	@Override
	public byte[] perform(T graph, Integer bufferSize) {
		if (graph == null) return EMPTY;
		else {
			if (bufferSize == null || bufferSize <1) bufferSize = DEFAULT_BUFFER;
			Kryo kryo = null;
			try {
				Output output = new Output(new ByteArrayOutputStream(), bufferSize);
				kryo = pool.borrow();
				kryo.writeObject(output, graph);
				byte[] serialized = output.toBytes();
				return serialized;
			} catch (Exception e) {
				log.warn("Fail to serializer graph. graph=" + graph, e);
				return EMPTY;
			} finally {
				if (kryo != null) pool.release(kryo);
			}
		}
	}

	@Override
	public byte[] process(T graph) {
		return perform(graph, DEFAULT_BUFFER);
	}
	
	public class InputProcessor implements IProcessor<byte[], Object>, IBiProcessor<byte[], Class<?>, Object> {
		@Override
		public Object perform(byte[] bytes, Class<?> clazz) {
			if (bytes != null && bytes.length > 0) {
				Kryo kryo = null;
				try {
					T obj;
					kryo = pool.borrow();
					Input input = new Input(bytes);
					obj = (T) kryo.readObject(input, clazz);
					return obj;
				} catch (Exception e) {
					log.warn("Fail to deserialize bytes.", e);
					return null;
				} finally {
					if (kryo != null) pool.release(kryo);
				}
			}
			return null;
		}

		@Override
		public Object process(byte[] bytes) {
			if (bytes != null && bytes.length > 0) {
				Kryo kryo = null;
				try {
					T obj;
					kryo = pool.borrow();
					Input input = new Input(bytes);
					obj = (T) kryo.readClassAndObject(input);
					return obj;
				} catch (Exception e) {
					log.warn("Fail to deserialize bytes.", e);
					return null;
				} finally {
					if (kryo != null) pool.release(kryo);
				}
			}
			return null;
		}
	}
}
