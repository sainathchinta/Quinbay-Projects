package com.gdn.partners.pbp.web.listener;

import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletContextListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;

import com.gdn.partners.pbp.service.sysparam.SystemParameterService;

/**
 * A listener class when the application is started on server. This listener must be placed after
 * org.springframework.web.context.ContextLoaderListener in web.xml
 * 
 * @author andrew.winata
 */
public class ApplicationStartupListener implements ServletContextListener {
  private static final Logger LOG = LoggerFactory.getLogger(ApplicationStartupListener.class);

  @Autowired
  @Qualifier("systemParameterService")
  private SystemParameterService systemParameterService;

  @Override
  public void contextInitialized(ServletContextEvent sce) {
    LOG.info("PBP is starting...");
    ServletContext servletContext = sce.getServletContext();
    WebApplicationContext ctx =
        WebApplicationContextUtils.getRequiredWebApplicationContext(servletContext);
    AutowireCapableBeanFactory beanFactory = ctx.getAutowireCapableBeanFactory();
    beanFactory.autowireBean(this);

    if (!systemParameterService.isInitialized()) {
      systemParameterService.reload(true);
    } else {
      systemParameterService.reload(false);
    }
  }

  @Override
  public void contextDestroyed(ServletContextEvent sce) {
    LOG.info("PBP stopped...");
  }

}
