package com.gdn.mta.product.web.filter;

import java.io.IOException;

import jakarta.servlet.ServletException;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockFilterConfig;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import com.gdn.mta.product.service.util.AuditTrailMandatoryRequestParameterUtil;

public class AuditTrailMandatoryRequestHolderFilterTest {

  private static final String DEFAULT_CLIENT_HOST = "localhost";

  private MockHttpServletRequest httpServletRequest;

  private MockHttpServletResponse httpServletResponse;

  private MockFilterChain filterChain;
  
  private MockFilterConfig filterConfig;

  private AuditTrailMandatoryRequestHolderFilter filter =
      new AuditTrailMandatoryRequestHolderFilter();

  @BeforeEach
  public void initializeTest() {
    httpServletRequest = new MockHttpServletRequest();
    httpServletResponse = new MockHttpServletResponse();
    filterChain = new MockFilterChain();
    filterConfig = new MockFilterConfig();
  }

  @Test
  public void testDoFilterSuccess() throws Exception {
    httpServletRequest.addParameter(AuditTrailMandatoryRequestParameterUtil.CLIENT_HOST_KEY,
        DEFAULT_CLIENT_HOST);
    filter.doFilter(httpServletRequest, httpServletResponse, filterChain);
  }
  
  @Test
  public void testDoFilter_WithoutClientHost() throws Exception {
    Assertions.assertThrows(ServletException.class, () -> {
      filter.doFilter(httpServletRequest, httpServletResponse, filterChain);
    });
  }
  
  @Test
  public void testInit() throws Exception{
    filter.init(filterConfig);
  }
  
  @Test
  public void testDestroy() throws Exception{
    filter.destroy();
  }
}
