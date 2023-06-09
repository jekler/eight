package net.yeeyaa.eight.data.util;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Properties;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SessionImplementor;
import org.hibernate.internal.util.ReflectHelper;
import org.hibernate.type.SerializationException;
import org.hibernate.usertype.ParameterizedType;
import org.hibernate.usertype.UserType;


public class PlatformUserType implements UserType, ParameterizedType, Serializable{
	protected Class<?> returnClass = Object.class;
	protected int type = Types.OTHER;
	protected Boolean mutable = false;
	protected Raw raw = Raw.Object;	
	protected IProcessor<Object, Object> marshal;
	protected IProcessor<Object, Object> unmarshal;	
	protected IProcessor<Object, Object> copy;
	protected IBiProcessor<String, Object, Object> bi;
	protected String classname;
	
	protected enum Raw{Object, Array, Ascii, Big, Binary, Blob, Boolean, Byte, Bytes, Character, Clob, Date, Double, Float, Int, Long, NCharacter, 
		NClob, NString, Ref, RowId, Short, SqlXml, Time, Timestamp, Url, String};
	
	protected class Processor implements IProcessor<Object, Object> {
		protected volatile int resume; 
		protected volatile IProcessor<Object, Object> processor;
		protected Object key;
		
		public Processor(Object key, int resume) {
			this.key = key;
			this.resume = resume;
		}

		@Override
		public Object process(Object instance) {
			if (resume >= 0) {
				Object processor = bi.perform(classname, key);
				if (processor instanceof IProcessor) this.processor = (IProcessor<Object, Object>) processor;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (processor != null) resume = -1;
					break;
					case 2: if (this.processor != null) resume = -1;
				}
			}
			if (processor == null) return instance;
			else return processor.process(instance);
		}
	}	
	
	public Object assemble(Serializable cached, Object owner) throws HibernateException { 
		return cached == null ? null : deepCopy(cached); 
	} 
	
	public Object deepCopy(Object value) throws HibernateException { 
		if (copy == null) return PlatformUtil.copy(value); 
		else return copy.process(value);
	} 
	
	public Serializable disassemble(Object value) throws HibernateException { 
		if (value == null) return null;
		Object deepCopy = deepCopy(value);
		if (!(deepCopy instanceof Serializable)) throw new SerializationException(String.format("deepCopy of %s is not serializable", value), null);
		return (Serializable) deepCopy;
	} 
	
	public boolean equals(Object arg0, Object arg1) throws HibernateException { 
		if (arg0 == null && arg1 == null) return true; 
		else if (arg0 == null) return false; 
		else return arg0.equals(arg1); 
	} 
	
	public int hashCode(Object object) throws HibernateException { 
		assert (object != null);
		return object.hashCode(); 
	} 
	
	public boolean isMutable() { 
		return mutable; 
	} 
	
	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, SessionImplementor session, Object owner) throws HibernateException, SQLException { 
		Object value;
		switch (raw) {
			case String: value = rs.getString(names[0]);
			break;
			case Array: value = rs.getArray(names[0]);
			break;
			case Ascii: value = rs.getAsciiStream(names[0]);
			break;
			case Big: value = rs.getBigDecimal(names[0]);
			break;
			case Binary: value = rs.getBinaryStream(names[0]);
			break;
			case Blob: value = rs.getBlob(names[0]);
			break;
			case Boolean: value = rs.getBoolean(names[0]);
			break;
			case Byte: value = rs.getByte(names[0]);
			break;
			case Bytes: value = rs.getBytes(names[0]);
			break;
			case Character: value = rs.getCharacterStream(names[0]);
			break;
			case Clob: value = rs.getClob(names[0]);
			break;
			case Date: value = rs.getDate(names[0]);
			break;
			case Double: value = rs.getDouble(names[0]);
			break;
			case Float: value = rs.getFloat(names[0]);
			break;
			case Int: value = rs.getInt(names[0]);
			break;
			case Long: value = rs.getLong(names[0]);
			break;
			case NCharacter: value = rs.getNCharacterStream(names[0]);
			break;
			case NClob: value = rs.getNClob(names[0]);
			break;
			case NString: value = rs.getNString(names[0]);
			break;
			case Ref: value = rs.getRef(names[0]);
			break;
			case RowId: value = rs.getRowId(names[0]);
			break;
			case Short: value = rs.getShort(names[0]);
			break;
			case SqlXml: value = rs.getSQLXML(names[0]);
			break;
			case Time: value = rs.getTime(names[0]);
			break;
			case Timestamp: value = rs.getTimestamp(names[0]);
			break;
			case Url: value = rs.getURL(names[0]);
			break;		
			default: value = rs.getObject(names[0]);
		}
		if (unmarshal != null) value = unmarshal.process(value);
		return value;
	} 

	@Override
	public void nullSafeSet(PreparedStatement st, Object value, int index, SessionImplementor session) throws HibernateException, SQLException { 
		if (value == null) st.setNull(index, Types.NULL); 
		else {
			if (marshal != null) value = marshal.process(value);
			st.setObject(index, value, type);
		} 
	} 
	
	public Object replace(Object original, Object target, Object owner) throws HibernateException { 
		return deepCopy(original); 
	} 
	
	public Class<?> returnedClass() { 
		return returnClass; 
	} 
	
	public int[] sqlTypes() { 
		return new int[] {type}; 
	}

	@Override
	public void setParameterValues(Properties parameters) {
		if (parameters != null && parameters.size() > 0) {
			ClassLoader classloader = getClass().getClassLoader();
			if (classloader instanceof IBiProcessor) {
				bi = (IBiProcessor<String, Object, Object>) classloader;
				classname = getClass().getName();
				String marshal = parameters.getProperty("marshal_0");
				if (marshal != null && marshal.trim().length() > 0) this.marshal = new Processor(marshal, 0);
				else {
					marshal = parameters.getProperty("marshal_1");
					if (marshal != null && marshal.trim().length() > 0) this.marshal = new Processor(marshal, 1);
					else {
						marshal = parameters.getProperty("marshal_2");
						if (marshal != null && marshal.trim().length() > 0) this.marshal = new Processor(marshal, 2);
						else {
							marshal = parameters.getProperty("marshal_3");
							if (marshal != null && marshal.trim().length() > 0) this.marshal = new Processor(marshal, 3);
						}
					}
				}
				String unmarshal = parameters.getProperty("unmarshal_0");
				if (unmarshal != null && unmarshal.trim().length() > 0) this.unmarshal = new Processor(unmarshal, 0);
				else {
					unmarshal = parameters.getProperty("unmarshal_1");
					if (unmarshal != null && unmarshal.trim().length() > 0) this.unmarshal = new Processor(unmarshal, 1);
					else {
						unmarshal = parameters.getProperty("unmarshal_2");
						if (unmarshal != null && unmarshal.trim().length() > 0) this.unmarshal = new Processor(unmarshal, 2);
						else {
							unmarshal = parameters.getProperty("unmarshal_3");
							if (unmarshal != null && unmarshal.trim().length() > 0) this.unmarshal = new Processor(unmarshal, 3);
						}
					}
				}
				String copy = parameters.getProperty("copy_0");	
				if (copy != null && copy.trim().length() > 0) this.copy = new Processor(copy, 0);
				else {
					copy = parameters.getProperty("copy_1");
					if (copy != null && copy.trim().length() > 0) this.copy = new Processor(copy, 1);
					else {
						copy = parameters.getProperty("copy_2");
						if (copy != null && copy.trim().length() > 0) this.copy = new Processor(copy, 2);
						else {
							copy = parameters.getProperty("copy_3");
							if (copy != null && copy.trim().length() > 0) this.copy = new Processor(copy, 3);
						}
					}
				}
			}
			final String clazz = parameters.getProperty("classname");
	    	if (clazz != null && clazz.trim().length() > 0) try{
	    		returnClass = classloader.loadClass(clazz);
	    	} catch (ClassNotFoundException e) {
	    		try {
	    			returnClass = ReflectHelper.classForName(clazz);
	    		} catch (ClassNotFoundException e1) {
	    			throw new IllegalArgumentException("Class: " + clazz + " is not a known class type.");
	    		}
	    	}
			final String mutable = parameters.getProperty("mutable");
			if (mutable != null) this.mutable = "true".equals(mutable);	
			final String raw = parameters.getProperty("raw");
			if (raw != null && raw.trim().length() > 0) this.raw = Raw.valueOf(raw.toString());
			final String type = parameters.getProperty("type");
			if (type != null && type.trim().length() > 0) this.type = new Integer(type.toString());
		}
	}
}
