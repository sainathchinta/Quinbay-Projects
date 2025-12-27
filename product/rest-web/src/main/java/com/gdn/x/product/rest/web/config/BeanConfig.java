package com.gdn.x.product.rest.web.config;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.Primary;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.enums.TracerFieldKey;
import com.gdn.x.product.service.interceptor.TracerHelperImpl;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.gson.GsonFactory;
import com.google.api.services.youtube.YouTube;
import brave.baggage.BaggageField;
import brave.baggage.BaggagePropagation;
import brave.baggage.BaggagePropagationConfig;
import brave.propagation.B3Propagation;
import brave.propagation.Propagation;
import feign.Retryer;
import jakarta.annotation.PostConstruct;


@Configuration
@EnableAutoConfiguration
@EnableAspectJAutoProxy
public class BeanConfig{

  private static final String APP_ID = "x-product";
  public static final String SLASH = "/";
  public static final String DEFAULT_HTTP_PARAMS = "maxConnections/20;maxConnectionsPerHost/10";

  @Bean
  public static PropertySourcesPlaceholderConfigurer placeholderConfigurer() {
    return new PropertySourcesPlaceholderConfigurer();
  }
  @Autowired
  private TracerHelperImpl traceHelper;

  @Bean
  public static YouTube youTube() {
    return new YouTube.Builder(new NetHttpTransport(), new GsonFactory(), request -> {
    }).setApplicationName(APP_ID).build();
  }

  @Bean
  public ModelMapper modelMapper() {
    return new ModelMapper();
  }

  @Bean
  public Propagation.Factory extraFieldPropagation() {
    BaggagePropagation.FactoryBuilder factoryBuilder =
      BaggagePropagation.newFactoryBuilder(B3Propagation.FACTORY);
    TracerFieldKey.getKeys().stream().map(BaggageField::create)
      .map(BaggagePropagationConfig.SingleBaggageField::local).forEach(factoryBuilder::add);
    return factoryBuilder.build();
  }

  @Bean
  @PostConstruct
  public Jackson2ObjectMapperBuilderCustomizer customizer() {
    return builder -> {
      ObjectMapper mapper = builder.build();
      mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
      builder.configure(mapper);
    };
  }

  @Bean
  @Primary
  public Retryer getFeignRetryer() {
    return new Retryer.Default();
  }
}
