package com.gdn.aggregate.platform.module.product.app;

import com.gdn.aggregate.AggregatePlatformFrameworkConfiguration;
import com.gdn.aggregate.platform.module.product.app.generator.UniqueBeanNameGenerator;
import com.gdn.aggregate.platform.properties.ElasticSearchProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.AutoConfigurationExcludeFilter;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.TypeExcludeFilter;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Import;

@EnableDiscoveryClient
@SpringBootApplication
@EnableConfigurationProperties({
    ElasticSearchProperties.class,
})
@Import(AggregatePlatformFrameworkConfiguration.class)
@ComponentScan(
    excludeFilters = {
        @ComponentScan.Filter(type = FilterType.CUSTOM, classes = TypeExcludeFilter.class),
        @ComponentScan.Filter(type = FilterType.CUSTOM, classes = AutoConfigurationExcludeFilter.class)
    },
    nameGenerator = UniqueBeanNameGenerator.class
)
public class ModuleDomainProductApp {

  public static void main(String[] args) {
    SpringApplication.run(ModuleDomainProductApp.class, args);
  }

}