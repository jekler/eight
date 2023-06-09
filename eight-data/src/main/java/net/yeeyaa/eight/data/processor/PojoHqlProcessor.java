package net.yeeyaa.eight.data.processor;

import java.lang.reflect.Method;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PojoHqlProcessor implements IProcessor<Object, String> {
	protected static final Logger log = LoggerFactory.getLogger(PojoHqlProcessor.class);
	protected String prefix;
	protected Boolean and = true;
	protected Boolean where = false;
	
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public void setAnd(Boolean and) {
		if(and != null) this.and = and;
	}

	public void setWhere(Boolean where) {
		if(where != null) this.where = where;
	}

	@Override
	public String process(Object in) {
		StringBuilder sb = new StringBuilder();
		if(in != null){
			String prefix = this.prefix;
			if(prefix == null) {
				prefix = in.getClass().getSimpleName();
				prefix = Character.toLowerCase(prefix.charAt(0)) + prefix.substring(1);
			}
			Method[] methods = in.getClass().getMethods();
			for(Method method : methods) try{
				String name = method.getName();
				if(name.startsWith("get") && method.getParameterTypes().length == 0){
					Object o = method.invoke(in);
					if(o != null) {
						name = Character.toLowerCase(name.charAt(3)) + name.substring(4);
						if(and) sb.append(" and ");
						else sb.append(" or ");
						sb.append(prefix).append(".").append(name).append(" = ");
						if(o instanceof Number || o instanceof Boolean) sb.append(o);
						else sb.append("'").append(o).append("'");
					}
				}
			}catch(Exception e){
				log.error("PojoHqlProcessor: parse pojo error " + in.getClass().getName(), e);
			}
			if(where && sb.length() > 0) sb.insert(0, " where 1 = 1");
		}
		return sb.toString();
	}
}
