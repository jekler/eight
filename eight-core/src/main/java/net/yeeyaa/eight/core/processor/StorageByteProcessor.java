package net.yeeyaa.eight.core.processor;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage.Method;


public class StorageByteProcessor implements IProcessor<IExtendable<Object>, byte[]>{
	protected Boolean check = false;
	protected Integer buffer = 8192;
	protected Long max = 0L;
	
	public void setCheck(Boolean check) {
		if(check != null) this.check = check;
	}

	public void setMax(Long max) {
		if(max != null && max > 0)  this.max = max;
	}

	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}

	@Override
	public byte[] process(IExtendable<Object> storage) {
		if(check && Boolean.FALSE.equals(storage.extend(Method.exists))) return null;
		InputStream is = null;
		try {
			is = storage.extend(Method.input);
			ByteArrayOutputStream b = new ByteArrayOutputStream(buffer);
			byte[] data = new byte[buffer];
			int nRead;
			Long count = 0L;
			while ((nRead = is.read(data, 0, data.length)) != -1)  {
				if (max > 0) {
					count += nRead;
					if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
				}
				b.write(data, 0, nRead);
			}
			return b.toByteArray();
		} catch (IOException e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, "StorageByteProcessor : read storage error.", e);
		}finally {
			try{
				if(is != null) is.close();
			} catch (IOException e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, "StorageByteProcessor : read storage error.", e);
			}
		}
	}
}
