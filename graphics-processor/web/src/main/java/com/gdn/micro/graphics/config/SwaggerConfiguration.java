package com.gdn.micro.graphics.config;


import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;


@Configuration
@EnableWebMvc
public class SwaggerConfiguration {

  @Value("${blibli.backend.version.version}")
  private String appVersion;

  @Autowired
  private CustomOperationCustomizer customOperationCustomizer;

  @Bean
  public GroupedOpenApi publicApi() {
    return GroupedOpenApi.builder().group("Merchant Tools Application (MTA)").pathsToMatch("/**/")
        .packagesToScan("com.gdn.micro.graphics.controller").displayName("Graphics Processor")
        .addOpenApiCustomizer(openApi -> bulkApi()).addOperationCustomizer(customOperationCustomizer).build();
  }

  @Bean
  public OpenAPI bulkApi() {
    return new OpenAPI().info(new Info().title("Graphics Processor").description("XGP").version(appVersion)
            .license(new License().name("Apache 2.0").url("http://springdoc.org")))
        .externalDocs(new ExternalDocumentation());
  }
}
