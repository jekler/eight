package net.yeeyaa.eight.data.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.dialect.H2Dialect;
import org.hibernate.dialect.function.AnsiTrimEmulationFunction;
import org.hibernate.dialect.function.CastFunction;
import org.hibernate.dialect.function.CharIndexFunction;
import org.hibernate.dialect.function.ConvertFunction;
import org.hibernate.dialect.function.DerbyConcatFunction;
import org.hibernate.dialect.function.NoArgSQLFunction;
import org.hibernate.dialect.function.NvlFunction;
import org.hibernate.dialect.function.PositionSubstringFunction;
import org.hibernate.dialect.function.SQLFunction;
import org.hibernate.dialect.function.SQLFunctionTemplate;
import org.hibernate.dialect.function.StandardSQLFunction;
import org.hibernate.dialect.function.VarArgsSQLFunction;
import org.hibernate.type.StandardBasicTypes;
import org.hibernate.type.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class H2SQLDialect extends H2Dialect {
	protected final static Logger log = LoggerFactory.getLogger(H2SQLDialect.class);
	protected final static Pattern column = Pattern.compile("((\\d+)_)?(\\w+)_(\\d+)");
	protected final static Pattern hibernate = Pattern.compile("((\\d+)_)?(\\d+)_(\\w+)");
	protected enum Function{keyword, ansi, cast, charindex, convert, derby, noarg, nv, substring, sql, standard, vararg, other}
	protected enum DbType{CHAR, VARCHAR, CLOB, VARCHAR_IGNORECASE, BINARY, VARBINARY, BLOB, BOOLEAN, TINYINT, SMALLINT, INT, BIGINT, NUMERIC, REAL, FLOAT,
		DECFLOAT, DATE, TIME, TIMESTAMP, JSON, UUID}
	
	public H2SQLDialect() {
		for (String kv : getClass().getName().split("\\.")) {
			try {
				Matcher matcher = column.matcher(kv);
				if (matcher.matches()) if (matcher.group(2) == null) registerColumnType(Integer.parseInt(matcher.group(4)), matcher.group(3));
				else registerColumnType(Integer.parseInt(matcher.group(4)), Long.parseLong(matcher.group(2)), matcher.group(3));
				else {
					matcher = hibernate.matcher(kv);
					if (matcher.matches()) if (matcher.group(2) == null) registerHibernateType(Integer.parseInt(matcher.group(3)), matcher.group(4));
					else registerHibernateType(Integer.parseInt(matcher.group(3)), Long.parseLong(matcher.group(2)), matcher.group(4));
					else {
						String[] funcs = kv.split("_");
						if (funcs.length > 1) switch(Function.valueOf(funcs[0])) {
							case keyword : registerKeyword(funcs[1]);
							break;
							case ansi : if (funcs.length > 4) registerFunction(funcs[1], new AnsiTrimEmulationFunction(funcs[2], funcs[3], funcs[4]));
							else registerFunction(funcs[1], new AnsiTrimEmulationFunction());
							break;
							case cast : registerFunction(funcs[1], new CastFunction());
							break;
							case charindex : registerFunction(funcs[1], new CharIndexFunction());
							break;
							case convert : registerFunction(funcs[1], new ConvertFunction());
							break;
							case derby : registerFunction(funcs[1], new DerbyConcatFunction());
							break;
							case noarg : if (funcs.length > 4) registerFunction(funcs[1], new NoArgSQLFunction(funcs[2], getType(funcs[3]), false));
							else registerFunction(funcs[1], new NoArgSQLFunction(funcs[2], getType(funcs[3])));
							break;
							case sql : if (funcs.length > 4) registerFunction(funcs[1], new SQLFunctionTemplate(getType(funcs[3]), funcs[2], false));
							else registerFunction(funcs[1], new SQLFunctionTemplate(getType(funcs[3]), funcs[2]));
							break;
							case standard : if (funcs.length > 2) registerFunction(funcs[1], new StandardSQLFunction(funcs[2], getType(funcs[3])));
							else registerFunction(funcs[1], new StandardSQLFunction(funcs[2]));
							break;
							case vararg : if (funcs.length > 5) registerFunction(funcs[1], new VarArgsSQLFunction(getType(funcs[5]), funcs[2], funcs[3], funcs[4]));
							else registerFunction(funcs[1], new VarArgsSQLFunction(funcs[2], funcs[3], funcs[4]));
							break;
							case nv : registerFunction(funcs[1], new NvlFunction());
							break;
							case substring : registerFunction(funcs[1], new PositionSubstringFunction());
							break;
							default : Object o = getClass().getClassLoader().loadClass(funcs[2]).newInstance();
							if (o instanceof SQLFunction) registerFunction(funcs[1], (SQLFunction)o);
						}			
					}
				}
			} catch (Exception e) {
				log.error("PostgreSQLDialect: init param error", e);
			}
		}
	}
	
	protected Type getType(String type) {
		switch (DbType.valueOf(type)) {
			case BOOLEAN : return StandardBasicTypes.BOOLEAN;
			case TINYINT : return StandardBasicTypes.BYTE;
			case SMALLINT : return StandardBasicTypes.SHORT;
			case INT : return StandardBasicTypes.INTEGER;
			case BIGINT : return StandardBasicTypes.LONG;
			case REAL : return StandardBasicTypes.FLOAT;
			case FLOAT : return StandardBasicTypes.DOUBLE;
			case NUMERIC : return StandardBasicTypes.BIG_DECIMAL;
			case DECFLOAT : return StandardBasicTypes.BIG_DECIMAL;
			case CHAR : return StandardBasicTypes.CHARACTER;
			case VARCHAR_IGNORECASE : return StandardBasicTypes.STRING;
			case VARCHAR : return StandardBasicTypes.STRING;			
			case CLOB : return StandardBasicTypes.CLOB;
			case TIME : return StandardBasicTypes.TIME;
			case DATE : return StandardBasicTypes.DATE;
			case TIMESTAMP : return StandardBasicTypes.TIMESTAMP;
			case BINARY : return StandardBasicTypes.BINARY;
			case VARBINARY : return StandardBasicTypes.BINARY;
			case BLOB : return StandardBasicTypes.BLOB;
			case UUID : return StandardBasicTypes.UUID_CHAR;
			case JSON : return StandardBasicTypes.STRING;
		}
		return null;
	}
}
