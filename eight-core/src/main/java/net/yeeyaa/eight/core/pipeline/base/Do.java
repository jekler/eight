package net.yeeyaa.eight.core.pipeline.base;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class Do implements IProcessor<Map<String, String>, Void> {
	protected final Logger log;
	protected String regex="\\|";
	protected IProcessor<Object, Object> beanHolder;
	
	public Do() {
		this.log = LoggerFactory.getLogger(Do.class);
	}

	public Do(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(Do.class) : log;
	}
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	@Override
	public Void process(Map<String, String> paras) {
		try {
			Object o = beanHolder.process(paras.get("bean"));
			String[] terminalset = paras.get("terminal").split(regex+regex);
			if(IProcessor.class.isInstance(o) && terminalset.length > 1){
				Map<String, String> newparas = new HashMap<String, String>();
				for(Entry<String, String> entry : paras.entrySet()) if(entry.getKey().indexOf("d:") == 0) newparas.put(entry.getKey().substring(2), entry.getValue());
				Boolean loop;
				do {
					((IProcessor<Map<String, String>, Void>) o).process(newparas);
					Object left = TypeConvertor.parseEntity(beanHolder, terminalset[0], regex);
					Object right = TypeConvertor.parseEntity(beanHolder, terminalset[1], regex);
					if(terminalset.length > 2) {
						if("gt".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) > 0;
						else if("lt".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) < 0;
						else if("ge".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) >= 0;
						else if("le".equals(terminalset[2])) loop = ((Comparable)left).compareTo(right) <= 0;
						else loop = left != right && (left == null || !left.equals(right));
					}else loop = left != right && (left == null || !left.equals(right));
				} while (loop);
			}
		} catch (Exception e) {
			log.error("Do: performing error.", e);
		}
		return null;
	}
}
