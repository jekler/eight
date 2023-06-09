package net.yeeyaa.eight.core.pipeline.base;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class While implements IProcessor<Map<String, String>, Void> {
	protected final Logger log;
	protected String regex="\\|";
	protected IProcessor<Object, Object> beanHolder;
	
	public While() {
		this.log = LoggerFactory.getLogger(While.class);
	}

	public While(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(While.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	@Override
	public Void process(Map<String, String> paras) {
		try {
			Object o = beanHolder.process((String)TypeConvertor.getParas(beanHolder, paras, "bean", regex));
			String[] terminalset = paras.get("terminal").split(regex+regex);
			if(IProcessor.class.isInstance(o) && terminalset.length > 1){
				HashMap<String, String> newparas = new HashMap<String, String>();
				for(Entry<String, String> entry : paras.entrySet()) if(entry.getKey().indexOf("w:") == 0) newparas.put(entry.getKey().substring(2), entry.getValue());
				Boolean loop;
				Object left = TypeConvertor.parseEntity(beanHolder, terminalset[0], regex);
				Object right = TypeConvertor.parseEntity(beanHolder, terminalset[1], regex);
				if(terminalset.length > 2) {
					if("gt".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) > 0;
					else if("lt".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) < 0;
					else if("ge".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) >= 0;
					else if("le".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) <= 0;
					else loop = left != right && (left == null || !left.equals(right));
				}else loop = left != right && (left == null || !left.equals(right));
				while (loop){
					((IProcessor<Map<String, String>, Void>) o).process(newparas);
					left = TypeConvertor.parseEntity(beanHolder, terminalset[0], regex);
					right = TypeConvertor.parseEntity(beanHolder, terminalset[1], regex);
					if(terminalset.length > 2) {
						if("gt".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) > 0;
						else if("lt".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) < 0;
						else if("ge".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) >= 0;
						else if("le".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) <= 0;
						else loop = left != right && (left == null || !left.equals(right));
					}else loop = left != right && (left == null || !left.equals(right));
				};
			}
		} catch (Exception e) {
			log.error("While: performing error.", e);
		}	
		return null;
	}
}
