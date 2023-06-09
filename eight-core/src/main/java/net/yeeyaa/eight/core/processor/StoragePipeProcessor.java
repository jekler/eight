package net.yeeyaa.eight.core.processor;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class StoragePipeProcessor implements IProcessor<Object, Boolean>, IBiProcessor<IExtendable<Object>, IExtendable<Object>, Boolean>, ITriProcessor<IExtendable<Object>, IExtendable<Object>, IProcessor<byte[], byte[]>, Boolean> {
	protected final Logger log;
	protected IProcessor<InputStream, InputStream> preProcessor;
	protected IProcessor<byte[], byte[]> processor;
	protected IProcessor<InputStream, InputStream> postProcessor;
	protected IProcessor<Object, IExtendable<Object>> srcProcessor;
	protected IProcessor<Object, IExtendable<Object>> destProcessor;
	protected IProcessor<IExtendable<Object>, Void> rollback;
	protected int buffer = 8192;
	protected Boolean type;
	protected Long max = 0L;
	
	public StoragePipeProcessor() {
		this.log = LoggerFactory.getLogger(StoragePipeProcessor.class);
	}
	
	public StoragePipeProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(StoragePipeProcessor.class) : log;
	}
	
	public void setMax(Long max) {
		if (max != null && max > 0) this.max = max;
	}

	public void setBuffer(Integer buffer) {
		if (buffer != null && buffer > 0) this.buffer = buffer;
	}

	public void setRollback(IProcessor<IExtendable<Object>, Void> rollback) {
		this.rollback = rollback;
	}

	public void setPreProcessor(IProcessor<InputStream, InputStream> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setProcessor(IProcessor<byte[], byte[]> processor) {
		this.processor = processor;
	}

	public void setPostProcessor(IProcessor<InputStream, InputStream> postProcessor) {
		this.postProcessor = postProcessor;
	}

	public void setSrcProcessor(IProcessor<Object, IExtendable<Object>> srcProcessor) {
		this.srcProcessor = srcProcessor;
	}

	public void setDestProcessor(IProcessor<Object, IExtendable<Object>> destProcessor) {
		this.destProcessor = destProcessor;
	}

	public void setType(Boolean type) {
		this.type = type;
	}

	@Override
	public Boolean process(Object instance) {
		IExtendable<Object> src = null;
		IExtendable<Object> dest = null;
		IProcessor<byte[], byte[]> processor = this.processor;
		if (type == null && instance instanceof Object[] && ((Object[])instance).length > 1) {
			if (srcProcessor != null) src = srcProcessor.process(((Object[])instance)[0]);
			else if (((Object[])instance)[0] instanceof IExtendable) src = (IExtendable<Object>) ((Object[])instance)[0];
			if (destProcessor != null) dest = destProcessor.process(((Object[])instance)[1]);
			else if (((Object[])instance)[1] instanceof IExtendable) dest = (IExtendable<Object>) ((Object[])instance)[1];
			if (((Object[])instance).length > 2 && ((Object[])instance)[2] instanceof IProcessor) processor = (IProcessor<byte[], byte[]>) ((Object[])instance)[2];
		} else if (srcProcessor != null && destProcessor != null) if (Boolean.TRUE.equals(type)) {
			src = srcProcessor.process(instance);
			dest = destProcessor.process(instance);
		} else if (instance instanceof Object[] && ((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof IProcessor) {
			processor = (IProcessor<byte[], byte[]>) ((Object[])instance)[1];
			src = srcProcessor.process(((Object[])instance)[0]);
			dest = destProcessor.process(((Object[])instance)[0]);
		}
		return operate(src, dest, processor);
	}

	@Override
	public Boolean perform(IExtendable<Object> src, IExtendable<Object> dest) {
		return operate(src, dest, processor);
	}

	@Override
	public Boolean operate(IExtendable<Object> src, IExtendable<Object> dest, IProcessor<byte[], byte[]> processor) {
		InputStream in = null;
		OutputStream out = null;
		if (src != null && dest != null) try {
			in = preProcessor == null ? src.<InputStream>extend(Method.input) : preProcessor.process(src.<InputStream>extend(Method.input));
			out = dest.<OutputStream>extend(Method.output);
			int nRead;
			byte[] data = new byte[buffer];
			Long count = 0L;
			while ((nRead = in.read(data, 0, data.length)) != -1) {
				byte[] buffer = data;
				if (processor != null) {
					buffer = processor.process(data);
					nRead = buffer.length;
				}
				if (max > 0) {
					count += nRead;
					if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
				}
				out.write(buffer, 0, nRead);
			}
			if (postProcessor != null) in = postProcessor.process(in);
			out.flush();
			return true;
		} catch (Exception e) {
			if (rollback != null) rollback.process(dest);
			log.error("StoragePipeProcessor: process fail.", e);
		} finally {
			try {
				if (in != null) in.close();
			} catch (Exception e) {
				log.error("StoragePipeProcessor: process fail.", e);
			} finally {
				try {
					if (out != null) out.close();
				} catch (Exception e) {
					log.error("StoragePipeProcessor: process fail.", e);
				}
			}
		}
		return false;
	}
	
	public class GenKey implements IProcessor<Object, Object>, IBiProcessor<IExtendable<Object>, IExtendable<Object>, Object>, ITriProcessor<IExtendable<Object>, IExtendable<Object>, IProcessor<byte[], byte[]>, Object> {
		protected IProcessor<Object, Object> key;
		
		public void setKey(IProcessor<Object, Object> key) {
			this.key = key;
		}

		@Override
		public Object operate(IExtendable<Object> src, IExtendable<Object> dest, IProcessor<byte[], byte[]> processor) {
			Object ret = key.process(dest);
			if (destProcessor != null) dest = destProcessor.process(ret);
			else if (ret instanceof IExtendable) dest = (IExtendable<Object>) ret;
			if (StoragePipeProcessor.this.operate(src, dest, processor)) return  ret;
			else throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL);
		}

		@Override
		public Object perform(IExtendable<Object> src, IExtendable<Object> dest) {
			return operate(src, dest, processor);
		}

		@Override
		public Object process(Object instance) {
			Object ret = key.process(instance);
			IExtendable<Object> src = null;
			IExtendable<Object> dest = destProcessor == null ? ret instanceof IExtendable ? (IExtendable<Object>) ret : null : destProcessor.process(ret);
			IProcessor<byte[], byte[]> processor = StoragePipeProcessor.this.processor;
			if (!Boolean.TRUE.equals(type) && instance instanceof Object[] && ((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof IProcessor) {		
				if (srcProcessor != null) src = srcProcessor.process(((Object[])instance)[0]);
				else if (((Object[])instance)[0] instanceof IExtendable) src = (IExtendable<Object>) ((Object[])instance)[0];
				processor = (IProcessor<byte[], byte[]>) ((Object[])instance)[1];
			} else if (!Boolean.FALSE.equals(type)) if (srcProcessor != null) src = srcProcessor.process(instance);
			else if (instance instanceof IExtendable) src = (IExtendable<Object>) instance;
			if (StoragePipeProcessor.this.operate(src, dest, processor)) return  ret;
			else throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL);
		}
	}
}
