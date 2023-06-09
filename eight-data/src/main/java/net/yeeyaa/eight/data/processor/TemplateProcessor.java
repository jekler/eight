package net.yeeyaa.eight.data.processor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITriProcessor;

import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.CallableStatementCallback;
import org.springframework.jdbc.core.ConnectionCallback;
import org.springframework.jdbc.core.JdbcOperations;
import org.springframework.jdbc.core.PreparedStatementCallback;
import org.springframework.jdbc.core.StatementCallback;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;


public class TemplateProcessor<T> implements IProcessor<Object, Object>, IBiProcessor<Object, Object, Object>, ITriProcessor<Object, Object, Class<T>, Object> {
	protected JdbcOperations template;
	protected NamedParameterJdbcOperations namedTemplate;
    protected Map<Object, String> map;
    protected Boolean single;
	
	public void setTemplate(Object template) {
		if (template instanceof NamedParameterJdbcOperations) {
			this.namedTemplate = (NamedParameterJdbcOperations) template;
			this.template = this.namedTemplate.getJdbcOperations();
		} else if (template instanceof JdbcOperations) this.template = (JdbcOperations) template;
	}

	public void setMap(Map<Object, String> map) {
		this.map = map;
	}

	public void setSql(String sql) {
		if (sql != null) {
			Map<Object, String> map = new HashMap<Object, String>();
	    	for(String s : sql.split("~~")){
	    		String[] ss = s.split("~");
	    		if(ss.length > 1){ 
	    			String key = ss[0].replace("`b", "~").replace("`c", "`").trim();
	    			String value = ss[1].replace("`b", "~").replace("`c", "`").trim();
	    			if(key.length() > 0 && value.length() > 0) map.put(key, value);
	    		}
	    	}
	    	this.map = map;
		}
	}

	public void setSingle(Boolean single) {
		this.single = single;
	}

	@Override
	public Object operate(Object query, Object args, Class<T> clazz) {
		if (query instanceof Object[]) {
			StringBuilder sb = new StringBuilder();
			for (Object q : (Object[])query) sb.append(' ').append(q);
			query = sb.substring(1);
		}
		String sql = map == null ? query.toString() : map.get(query);
		if (sql == null) return null;
		else {
			List result;
			if (args == null) if (clazz == null) result = namedTemplate == null ? template.queryForList(sql) : namedTemplate.queryForList(sql, Collections.EMPTY_MAP);
			else result = namedTemplate == null ? template.query(sql, new BeanPropertyRowMapper(clazz)) : namedTemplate.query(sql, new BeanPropertyRowMapper(clazz));
			else if (clazz == null) result = namedTemplate == null ? template.queryForList(sql, (Object[])args) : namedTemplate.queryForList(sql, (Map<String, ?>)args);
			else result = namedTemplate == null ? template.query(sql, (Object[])args, new BeanPropertyRowMapper(clazz)) : namedTemplate.query(sql, (Map<String, ?>)args, new BeanPropertyRowMapper(clazz));
			if (single == null) switch(result.size()) {
				case 0 : return null;
				case 1 : return result.get(0);
			} else if (single) if (result.size() > 0) return result.get(0);
			else return null;
			return result;
		}
	}

	@Override
	public Object perform(Object query, Object args) {
		if (query instanceof Object[]) {
			Object[] sqls = (Object[]) query;
			ArrayList<String> ss = new ArrayList<String>(sqls.length);
			for (Object sql : sqls) {
				String s = map == null ? sql.toString() : map.get(sql);
				if (s != null) ss.add(s);
			}
			return template.batchUpdate(ss.toArray(new String[ss.size()]));
		} else {
			String sql = map == null ? query.toString() : map.get(query);
			if (sql == null) return null;
			else if (args == null) return namedTemplate == null ? template.update(sql) : namedTemplate.update(sql, Collections.EMPTY_MAP);
			else if (args instanceof Object[]) return namedTemplate == null ? template.update(sql, (Object[])args) : namedTemplate.batchUpdate(sql, (Map<String, ?>[])args);
			else if (args instanceof Map) return namedTemplate.update(sql, (Map<String, ?>) args);
			else if (args instanceof List) return template.batchUpdate(sql, (List<Object[]>) args);
			else return template.update(sql, args);
		}
	}

	@Override
	public Object process(Object query) {
		if (query instanceof Object[]) {
			Object[] params = (Object[]) query;
			if (params.length > 1) {
				String sql = map == null ? params[0].toString() : map.get(params[0]);
				if (sql != null) if (params[1] instanceof CallableStatementCallback) return template.execute(sql, (CallableStatementCallback<Object>) params[1]);
				else if (params[1] instanceof PreparedStatementCallback) if (params.length > 2 && params[2] instanceof Map) return namedTemplate.execute(sql, (Map<String, ?>) params[2], (PreparedStatementCallback<Object>) params[1]);
				else return namedTemplate == null ? template.execute(sql, (PreparedStatementCallback<Object>) params[1]) : namedTemplate.execute(sql, (PreparedStatementCallback<Object>) params[1]);
			}
		} else if (query instanceof ConnectionCallback) return template.execute((ConnectionCallback<Object>) query);
		else if (query instanceof StatementCallback) return template.execute((StatementCallback<Object>) query);
		else {
			String sql = map == null ? query.toString() : map.get(query);
			if (sql != null) template.execute(sql);
		}
		return null;
	}  
}
