package net.yeeyaa.eight.data.processor;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.springframework.jdbc.core.ColumnMapRowMapper;
import org.springframework.jdbc.core.RowMapperResultSetExtractor;


public class DataSourceProcessor implements IProcessor<Object, Object>, IBiProcessor<Boolean, String, Object>, ITriProcessor<String, String, String, List<Map<String, Object>>> {
	protected DataSource source;
    protected String user;
    protected String password;
   
    public void setUser(String user) {
		this.user = user;
	}

	public void setSource(DataSource source) {
		this.source = source;
	}

	public void setPassword(String password) {
		this.password = password;
	}
	
	protected Object query(Boolean type, String user, String password, String sql) {
		Connection con = null;
        Statement st = null;
        ResultSet rs = null;
		try {
			con = user == null ? source.getConnection() : source.getConnection(user, password);
			if (type == null) {
				rs = con.createStatement().executeQuery(sql);
				 return new RowMapperResultSetExtractor<Map<String, Object>>(new ColumnMapRowMapper()).extractData(rs);
			} else if (type) return con.createStatement().execute(sql);
			else return con.createStatement().executeUpdate(sql);
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		} finally {
            try {
            	if (rs != null) rs.close();
            } catch (Exception e) {
    			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
    		} finally {
                try {
                	if (st != null) st.close();
                } catch (Exception e) {
        			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
        		} finally {
                    if (con != null) try {
                    	con.close();
                    } catch (Exception e) {
            			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
            		}
                }
            }
        }
	}

	@Override
	public Object process(Object parameter) {
		if (parameter != null) try {
			if (parameter instanceof Object[]) {
				Object[] paras = (Object[])parameter;
				if (paras.length > 0) switch (paras.length) {
					case 1: return operate(user, password, (String)paras[0]);
					case 2: if (paras[0] instanceof Boolean) query((Boolean)paras[0], user, password, (String)paras[1]);
						else return operate((String)paras[1], password, (String)paras[0]);
					case 3: if (paras[0] instanceof Boolean) query((Boolean)paras[0],  (String)paras[2], password, (String)paras[1]);
						else return operate((String)paras[1], (String)paras[2], (String)paras[0]);
					default: query((Boolean)paras[0],  (String)paras[2], (String)paras[3], (String)paras[1]);
				}
			} else return operate(user, password, parameter.toString());
		} catch (PlatformException e) {
			throw e;
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return null;
	}

	@Override
	public Object perform(Boolean type, String sql) {
		return query(type, user, password, sql);
	}
	
	@Override
    public List<Map<String, Object>> operate(String user, String password, String sql) {
		return (List<Map<String, Object>>) query(null, user, password, sql);
	}
}
