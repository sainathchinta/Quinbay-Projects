package com.gdn.mta.product.config;

import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import com.gdn.mta.product.enums.TracerFieldKey;
import brave.baggage.BaggageField;
import brave.baggage.BaggagePropagation;
import brave.baggage.BaggagePropagationConfig;
import brave.propagation.B3Propagation;
import brave.propagation.Propagation;

@Component
public class TracerConfiguration {

  @Bean
  public Propagation.Factory extraFieldPropagation() {
    BaggagePropagation.FactoryBuilder factoryBuilder =
        BaggagePropagation.newFactoryBuilder(B3Propagation.FACTORY);
    TracerFieldKey.getKeys().stream().map(BaggageField::create)
        .map(BaggagePropagationConfig.SingleBaggageField::local).forEach(factoryBuilder::add);
    return factoryBuilder.build();
  }
}
