package net.yeeyaa.eight.client;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.client.data.ClientMsg;


public class AuthClient<K, V, R> extends ServiceClient<K, V, R>{
	protected String login = "login";
	protected String logout = "logout";
	protected IProcessor<Object, Object> credential;
	protected IProcessor<Object, Object> parameter;
	protected DefaultError sessionout = new DefaultError("ServiceError", null, "NO_RIGHT_TO_ACCESS_THE_RESOURCE");
	protected Boolean auto = true;

	public void setLogin(String login) {
		if(login != null) this.login = login;
	}

	public void setLogout(String logout) {
		if(logout != null) this.logout = logout;
	}

	public void setCredential(IProcessor<Object, Object> credential) {
		this.credential = credential;
	}

	public void setParameter(IProcessor<Object, Object> parameter) {
		this.parameter = parameter;
	}

	public void setSessionout(DefaultError sessionout) {
		if(sessionout != null) this.sessionout = sessionout;
	}

	public void setAuto(Boolean auto) {
		if(auto != null) this.auto = auto;
	}

	public Boolean login(){
		return login(credential.process(this));
	}
	
	public Boolean login(Object credential){
		if(credential != null) try{
			ClientMsg msg = new ClientMsg();
			msg.name = login;
			msg.content = credential;
			return Boolean.TRUE.equals(super.process(msg));
		}catch(Exception e){}
		return false;
	}
	
	public Boolean logout(Object parameter){
		try{
			ClientMsg msg = new ClientMsg();
			msg.name = logout;
			msg.content = parameter;
			return Boolean.TRUE.equals(super.process(msg));
		}catch(Exception e){
			return false;
		}
	}
	
	public Boolean logout(){
		if(parameter != null) return logout(parameter.process(this));
		else return logout(null);
	}

	@Override
	public R process(ClientMsg request) {
		R ret = null;
		try{
			ret = super.process(request);
		}catch(PlatformException e){
			if(auto && sessionout.equals(e.getType()) && login()) ret = super.process(request);
			else throw e;
		}
		return ret;
	}
}
