package com.gdn.partners.pcu.master.web.config;

import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.pcu.master.service.AttributeService;
import com.gdn.partners.pcu.master.service.AutoQcConfigService;
import com.gdn.partners.pcu.master.service.CatalogService;
import com.gdn.partners.pcu.master.service.CategoryService;

/**
 * Created by govind on 20/11/2018 AD.
 */
@Configuration
public class TestConfiguration {

  @Bean
  public CatalogService getCatalogService() {
    return Mockito.mock(CatalogService.class);
  }

  @Bean
  public AttributeService getAttributeService() {
    return Mockito.mock(AttributeService.class);
  }

  @Bean
  public CategoryService getcategoryService() {
    return Mockito.mock(CategoryService.class);
  }

  @Bean
  public AutoQcConfigService getAutoQcConfigService() {
    return Mockito.mock(AutoQcConfigService.class);
  }

}
