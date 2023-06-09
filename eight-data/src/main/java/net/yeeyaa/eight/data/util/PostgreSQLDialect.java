package net.yeeyaa.eight.data.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.dialect.PostgreSQL9Dialect;
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


public class PostgreSQLDialect extends PostgreSQL9Dialect {
	protected final static Logger log = LoggerFactory.getLogger(PostgreSQLDialect.class);
	protected final static Pattern column = Pattern.compile("((\\d+)_)?(\\w+)_(\\d+)");
	protected final static Pattern hibernate = Pattern.compile("((\\d+)_)?(\\d+)_(\\w+)");
	protected enum Function{keyword, ansi, cast, charindex, convert, derby, noarg, nv, substring, sql, standard, vararg, other}
	protected enum DbType{BOOLEAN, NUMERICBOOLEAN, TRUEFALSE, YESNO, BYTE, SHORT, INTEGER, LONG, FLOAT, DOUBLE, BIGINTEGER, BIGDECIMAL, CHARACTER, STRING, URL, TIME, DATE, TIMESTAMP, CALENDAR, CALENDARDATE, CLASS, LOCALE, CURRENCY,
		TIMEZONE, UUIDBINARY, UUIDCHAR, BINARY, WRAPPERBINARY, IMAGE, BLOB, MATERIALIZEDBLOB, CHARARRAY, CHARACTERARRAY, TEXT, NTEXT, CLOB, NCLOB, MATERIALIZEDCLOB, MATERIALIZEDNCLOB, SERIALIZABLE}
	
	public PostgreSQLDialect() {
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
			case NUMERICBOOLEAN : return StandardBasicTypes.NUMERIC_BOOLEAN;
			case TRUEFALSE : return StandardBasicTypes.TRUE_FALSE;
			case YESNO : return StandardBasicTypes.YES_NO;
			case BYTE : return StandardBasicTypes.BYTE;
			case SHORT : return StandardBasicTypes.SHORT;
			case INTEGER : return StandardBasicTypes.INTEGER;
			case LONG : return StandardBasicTypes.LONG;
			case FLOAT : return StandardBasicTypes.FLOAT;
			case DOUBLE : return StandardBasicTypes.DOUBLE;
			case BIGINTEGER : return StandardBasicTypes.BIG_INTEGER;
			case BIGDECIMAL : return StandardBasicTypes.BIG_DECIMAL;
			case CHARACTER : return StandardBasicTypes.CHARACTER;
			case STRING : return StandardBasicTypes.STRING;
			case URL : return StandardBasicTypes.URL;
			case TIME : return StandardBasicTypes.TIME;
			case DATE : return StandardBasicTypes.DATE;
			case TIMESTAMP : return StandardBasicTypes.TIMESTAMP;
			case CALENDAR : return StandardBasicTypes.CALENDAR;
			case CALENDARDATE : return StandardBasicTypes.CALENDAR_DATE;
			case CLASS : return StandardBasicTypes.CLASS;
			case LOCALE : return StandardBasicTypes.LOCALE;
			case CURRENCY : return StandardBasicTypes.CURRENCY;
			case TIMEZONE : return StandardBasicTypes.TIMEZONE;
			case UUIDBINARY : return StandardBasicTypes.UUID_BINARY;
			case UUIDCHAR : return StandardBasicTypes.UUID_CHAR;
			case BINARY : return StandardBasicTypes.BINARY;
			case WRAPPERBINARY : return StandardBasicTypes.WRAPPER_BINARY;
			case IMAGE : return StandardBasicTypes.IMAGE;
			case BLOB : return StandardBasicTypes.BLOB;
			case MATERIALIZEDBLOB : return StandardBasicTypes.MATERIALIZED_BLOB;
			case CHARARRAY : return StandardBasicTypes.CHAR_ARRAY;
			case CHARACTERARRAY : return StandardBasicTypes.CHARACTER_ARRAY;
			case TEXT : return StandardBasicTypes.TEXT;
			case NTEXT : return StandardBasicTypes.NTEXT;
			case CLOB : return StandardBasicTypes.CLOB;
			case NCLOB : return StandardBasicTypes.NCLOB;
			case MATERIALIZEDCLOB : return StandardBasicTypes.MATERIALIZED_CLOB;
			case MATERIALIZEDNCLOB : return StandardBasicTypes.MATERIALIZED_NCLOB;
			case SERIALIZABLE : return StandardBasicTypes.SERIALIZABLE;
		}
		return null;
	}
}
