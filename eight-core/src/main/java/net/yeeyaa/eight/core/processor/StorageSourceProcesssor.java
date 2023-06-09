package net.yeeyaa.eight.core.processor;

import java.io.InputStream;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage.Method;


public class StorageSourceProcesssor implements IProcessor<Object, Source> {
	protected String systemId;
	protected String publicId; 

	public void setSystemId(String systemId) {
		this.systemId = systemId;
	}

	public void setPublicId(String publicId) {
		this.publicId = publicId;
	}

	@Override
	public Source process(Object instance) {
		if (instance != null) try {
			if (instance instanceof IExtendable) {
				StreamSource source = new StreamSource();
				source.setInputStream(((IExtendable<Object>)instance).<InputStream>extend(Method.input));
				if (systemId != null) source.setSystemId(systemId);
				if (publicId != null) source.setPublicId(publicId);
				return source;
			} else if (instance instanceof Object[] && ((Object[])instance).length > 0 && ((Object[])instance)[0] instanceof StreamSource) {
				StreamSource source = new StreamSource();
				source.setInputStream(((IExtendable<Object>)((Object[])instance)[0]).<InputStream>extend(Method.input));
				if (((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof String) source.setSystemId(((Object[])instance)[1].toString());
				if (((Object[])instance).length > 2 && ((Object[])instance)[2] instanceof String) source.setPublicId(((Object[])instance)[2].toString());
				return source;
			}
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return null;
	}
}
