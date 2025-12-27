package com.gdn.mta.product.web.filter;

import java.io.IOException;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;

import org.slf4j.MDC;
import org.springframework.util.StringUtils;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.service.util.AuditTrailMandatoryRequestParameterUtil;
import com.gdn.mta.product.web.param.AuditTrailMandatoryRequestParam;

public class AuditTrailMandatoryRequestHolderFilter implements Filter {

  private FilterConfig filterConfig;

  private void clear() {
    MDC.remove(AuditTrailMandatoryRequestParameterUtil.CLIENT_HOST_KEY);
  }
  
  private void insert(AuditTrailMandatoryRequestParam param) {
    MDC.put(AuditTrailMandatoryRequestParameterUtil.CLIENT_HOST_KEY, param.getClientHost());
  }

  @Override
  public void init(FilterConfig filterConfig) throws ServletException {
    this.filterConfig = filterConfig;
  }

  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain filterChain)
      throws IOException, ServletException {
    HttpServletRequest httpRequest = (HttpServletRequest) request;
    try {
      String clientHost =
          httpRequest.getParameter(AuditTrailMandatoryRequestParameterUtil.CLIENT_HOST_KEY);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(clientHost),
          "clientHost parameter must be specified");
      AuditTrailMandatoryRequestParam param = AuditTrailMandatoryRequestParam.builder().clientHost(clientHost).build();
      insert(param);
    } catch (Exception e) {
      throw new ServletException("mandatory request param is not valid", e);
    }
    try {
      filterChain.doFilter(request, response);
    } finally {
      this.clear();
    }
  }

  @Override
  public void destroy() {
    // TODO Auto-generated method stub

  }

}
