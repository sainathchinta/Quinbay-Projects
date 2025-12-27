package com.gdn.partners.pcu.master.web.configuration;

import com.gdn.partners.core.security.Credential;
import java.io.IOException;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.filter.OncePerRequestFilter;

public class CredentialFilter extends OncePerRequestFilter {
  public CredentialFilter() {
  }

  protected void doFilterInternal(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse, FilterChain filterChain) throws
      ServletException, IOException {
    Credential.setStoreId(httpServletRequest.getParameter("storeId"));
    Credential.setChannelId(httpServletRequest.getParameter("channelId"));
    Credential.setClientId(httpServletRequest.getParameter("clientId"));
    Credential.setRequestId(httpServletRequest.getParameter("requestId"));
    Credential.setUsername(httpServletRequest.getParameter("username"));
    filterChain.doFilter(httpServletRequest, httpServletResponse);
  }
}
