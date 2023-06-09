package net.yeeyaa.eight.data.util;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.Properties;

import org.hibernate.HibernateException;
import org.hibernate.MappingException;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.spi.SessionImplementor;
import org.hibernate.id.Configurable;
import org.hibernate.id.IdentifierGenerator;
import org.hibernate.internal.util.ReflectHelper;
import org.hibernate.type.Type;

public class IDGenerator implements IdentifierGenerator, Configurable {
	protected IdentifierGenerator generator;
	protected String id;
	protected Method method;

	@Override
	public void configure(Type type, Properties params, Dialect d)	throws MappingException {
		id = (String)params.get("id");
		String clazz = (String)params.get("generator");
		try {
			generator = (IdentifierGenerator) getClass().getClassLoader().loadClass(clazz).newInstance();
		} catch (Exception e) {
			try {
				generator = (IdentifierGenerator) ReflectHelper.classForName(clazz).newInstance();
			} catch (Exception ex) {
				throw new MappingException(ex);
			}
		}
		if (generator instanceof Configurable) ((Configurable)generator).configure(type, params, d);
	}

	@Override
	public Serializable generate(SessionImplementor session, Object object)	throws HibernateException {
		Serializable ret = null;
		if (object != null && id != null) try {
			if (method == null) method = object.getClass().getMethod(id);
			ret = (Serializable)method.invoke(object);
		} catch (Exception ex) {
			throw new HibernateException(ex);
		}
		return ret == null ? generator.generate(session, object) : ret;
	}
}
