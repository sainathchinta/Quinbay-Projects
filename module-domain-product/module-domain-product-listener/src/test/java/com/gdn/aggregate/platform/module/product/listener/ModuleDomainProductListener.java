package com.gdn.aggregate.platform.module.product.listener;

import com.gdn.aggregate.AggregatePlatformFrameworkConfiguration;
import com.gdn.aggregate.platform.properties.ElasticSearchProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Import;

@SpringBootApplication
@EnableConfigurationProperties({
    ElasticSearchProperties.class,
})
@Import(AggregatePlatformFrameworkConfiguration.class)
public class ModuleDomainProductListener {

  public static void main(String[] args) {
    SpringApplication.run(ModuleDomainProductListener.class, args);
  }

}