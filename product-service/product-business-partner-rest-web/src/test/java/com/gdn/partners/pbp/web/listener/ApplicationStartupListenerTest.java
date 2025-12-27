package com.gdn.partners.pbp.web.listener;

import jakarta.servlet.ServletContextEvent;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.mock.web.MockServletContext;
import org.springframework.web.context.WebApplicationContext;

import com.gdn.partners.pbp.service.sysparam.SystemParameterService;

public class ApplicationStartupListenerTest {

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  AutowireCapableBeanFactory autowireFactoryBean;

  @Mock
  WebApplicationContext webContext;

  MockServletContext mockServletContext;

  @InjectMocks
  private ApplicationStartupListener listener;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    mockServletContext = new MockServletContext();
    mockServletContext.setAttribute(WebApplicationContext.ROOT_WEB_APPLICATION_CONTEXT_ATTRIBUTE,
        webContext);
    Mockito.when(webContext.getServletContext()).thenReturn(mockServletContext);
    Mockito.when(webContext.getAutowireCapableBeanFactory()).thenReturn(autowireFactoryBean);
  }

  @Test
  public void testContextInitialized_ReloadSysparamTrue() {
    Mockito.when(systemParameterService.isInitialized()).thenReturn(false);
    listener.contextInitialized(new ServletContextEvent(mockServletContext));
    Mockito.verify(systemParameterService).reload(true);
  }

  @Test
  public void testContextInitialized_ReloadSysparamFalse() {
    Mockito.when(systemParameterService.isInitialized()).thenReturn(true);
    listener.contextInitialized(new ServletContextEvent(mockServletContext));
    Mockito.verify(systemParameterService).reload(false);
  }

  @Test
  public void testContextDestroyed() {
    listener.contextDestroyed(new ServletContextEvent(mockServletContext));
  }
}
