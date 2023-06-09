package net.yeeyaa.eight;

public class PlatformException extends RuntimeException {
	private static final long serialVersionUID = 192270499127845354L;
	protected Type type;
	
	public interface Type{
		public String getCate();
		public void setCate(String cate);		
		public String getMessage();
		public void setMessage(String message);
		public Integer getCode();
		public void setCode(Integer code);
	};
	
	public static class DefaultError implements Type {
		protected String message;
		protected Integer code;
		protected String cate;
		
		public DefaultError() {}

		public DefaultError(String cate, Integer code, String message) {
			this.message = message;
			this.code = code;
			this.cate = cate;
		}

		public String getCate() {
			return cate;
		}

		public void setCate(String cate) {
			this.cate = cate;
		}

		@Override
		public String getMessage() {
			return message;
		}

		@Override
		public void setMessage(String message) {
			this.message = message;
		}

		@Override
		public Integer getCode() {
			return code;
		}

		@Override
		public void setCode(Integer code) {
			this.code = code;
		}
		
		@Override
		public boolean equals(Object obj) {
			if(this == obj) return true;
			else if(obj instanceof Type){
				Type other = (Type) obj;
				return ((message == other.getMessage() ||(message != null && message.equals(other.getMessage()))) && 
						(code == other.getCode() || (code != null && code.equals(other.getCode()))) &&
						(cate == other.getCate() || (cate != null && cate.equals(other.getCate()))));
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			int hash = 0;
			if(message != null) hash = message.hashCode();
			if(code != null) hash = hash * 13 + code.hashCode();
			if(cate != null) hash = hash * 19 + cate.hashCode();
			return hash;
		}
	}
	
	public PlatformException(Type errType) {
		this.type = errType;
	}
	
	public PlatformException(Type errType, String addMsg) {
		super(addMsg);
		this.type = errType;
	}
	
	public PlatformException(Type errType, Throwable error) {
		super(error);	
		this.type = errType;
	}
	
	public PlatformException(Type errType, String addMsg, Throwable error) {
		super(addMsg, error);
		this.type = errType;
	}

	public Type getType() {
		return type;
	}	
}
