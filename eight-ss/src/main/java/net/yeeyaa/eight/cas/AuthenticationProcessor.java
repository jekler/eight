package net.yeeyaa.eight.cas;

import java.security.GeneralSecurityException;
import java.util.Map.Entry;

import javax.security.auth.login.AccountNotFoundException;
import javax.security.auth.login.FailedLoginException;

import org.jasig.cas.authentication.AuthenticationHandler;
import org.jasig.cas.authentication.BasicCredentialMetaData;
import org.jasig.cas.authentication.Credential;
import org.jasig.cas.authentication.DefaultHandlerResult;
import org.jasig.cas.authentication.HandlerResult;
import org.jasig.cas.authentication.PreventedException;
import org.jasig.cas.authentication.UsernamePasswordCredential;
import org.jasig.cas.authentication.principal.DefaultPrincipalFactory;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.Content.Couple;


public class AuthenticationProcessor implements AuthenticationHandler, IProcessor<UsernamePasswordCredential, Boolean> {
	protected IProcessor<Entry<Object, Object>, Boolean> authProcessor;
	protected DefaultPrincipalFactory pf = new DefaultPrincipalFactory();
	
	public void setAuthProcessor(IProcessor<Entry<Object, Object>, Boolean> authProcessor) {
		this.authProcessor = authProcessor;
	}

	@Override
	public Boolean process(UsernamePasswordCredential userPass) {
		Entry<Object, Object> cc = new Couple<Object, Object>(userPass.getUsername(), userPass.getPassword());
		 return authProcessor.process(cc);
	}

	@Override
	public HandlerResult authenticate(Credential credential) throws GeneralSecurityException, PreventedException {
		if (credential instanceof UsernamePasswordCredential) {
	         final UsernamePasswordCredential userPass = (UsernamePasswordCredential) credential;
	         if (userPass.getUsername() == null || userPass.getPassword() == null) throw new AccountNotFoundException("Username is null.");
	         else if (process(userPass)) return new DefaultHandlerResult(this, new BasicCredentialMetaData(credential), pf.createPrincipal(credential.getId()));
		}
		throw new FailedLoginException("Can not auth! username or password mismatch.");
	}

	@Override
	public boolean supports(Credential credential) {
		return credential instanceof UsernamePasswordCredential;
	}

	@Override
	public String getName() {
		return "net.yeeyaa.eight.cas.AuthenticationProcessor";
	}
}
