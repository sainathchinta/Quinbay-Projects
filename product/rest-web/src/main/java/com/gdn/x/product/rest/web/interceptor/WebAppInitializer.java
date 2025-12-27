package com.gdn.x.product.rest.web.interceptor;

import com.gdn.x.product.rest.web.ProductApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

public class WebAppInitializer extends SpringBootServletInitializer {

  @Override
  protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
    return application.sources(ProductApplication.class);
  }

}
