package com.gdn.aggregate.platform.module.product.listener.properties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Configuration
@ConfigurationProperties("price")
public class PriceProperties {

  private String fbbPriority;

  private String nonFbbPriority;

}
