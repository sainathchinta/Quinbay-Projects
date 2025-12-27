package com.gdn.partners.pcu.external.configuration;


import brave.baggage.BaggageField;
import brave.baggage.BaggagePropagation;
import brave.baggage.BaggagePropagationConfig;
import brave.propagation.B3Propagation;
import brave.propagation.Propagation;
import com.gdn.partners.pcu.external.web.configuration.TracerFieldKey;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Slf4j
@Configuration
public class TracerBeanConfig {
  @Bean
  public Propagation.Factory extraFieldPropagation() {
    BaggagePropagation.FactoryBuilder factoryBuilder =
        BaggagePropagation.newFactoryBuilder(B3Propagation.FACTORY);
    TracerFieldKey.getKeys().stream().map(BaggageField::create)
        .map(BaggagePropagationConfig.SingleBaggageField::local).forEach(factoryBuilder::add);
    return factoryBuilder.build();
  }
}
