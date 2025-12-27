package com.gdn.aggregate.platform.module.product.listener.properties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Configuration
@ConfigurationProperties("module.domain.product.republish")
public class RepublishProperties {

  private Set<String> lkppTopics;

  private Set<String> productItemTopics;

  private Set<String> productItemMigrationTopics;

  private int maxAttempt;

  public void setLkppTopics(List<String> lkppTopics) {
    this.lkppTopics = new HashSet<>(lkppTopics);
  }

  public void setProductItemTopics(List<String> productItemTopics) {
    this.productItemTopics = new HashSet<>(productItemTopics);
  }

  public void setProductItemMigrationTopics(List<String> productItemMigrationTopics) {
    this.productItemMigrationTopics = new HashSet<>(productItemMigrationTopics);
  }

  public void setMaxAttempt(int maxAttempt) {
    this.maxAttempt = maxAttempt;
  }

}
