package net.yeeyaa.eight.core.util;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;


public class BeanURLStreamHandler extends URLStreamHandler implements IProcessor<URL, URLConnection> { 
	protected IProcessor<Object, Object> beanHolder;
 
    public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public URLConnection openConnection(URL url) throws IOException { 
		URLConnection conn = process(url);
		if (conn == null) throw new IOException("BeanURLStreamHandler: no such resource." + url);
		else return conn;
    }

	@Override
	public URLConnection process(URL url) {
		if (url != null) {
			Object o = beanHolder.process(url.getHost());
			if (o instanceof URLConnection) return (URLConnection) o;
			else if (o instanceof ITriProcessor) {
				Object ret = ((ITriProcessor<Object, Object, Object, Object>) o).operate(url.getPath(), url.getQuery(), url.getRef());
				if (ret instanceof URLConnection) return (URLConnection) ret;
			} else if (o instanceof IBiProcessor) {
				Object ret = ((IBiProcessor<Object, Object, Object>) o).perform(url.getPath(), url.getQuery());
				if (ret instanceof URLConnection) return (URLConnection) ret;
			} else if (o instanceof IProcessor) {
				Object ret = ((IProcessor<Object, Object>) o).process(url.getFile());
				if (ret instanceof URLConnection) return (URLConnection) ret;
			}
		}
		return null;
	} 
}
