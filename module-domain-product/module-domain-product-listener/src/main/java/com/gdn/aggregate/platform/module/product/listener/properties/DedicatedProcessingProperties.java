package com.gdn.aggregate.platform.module.product.listener.properties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.collections.CollectionUtils;
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
@ConfigurationProperties("dedicated.processing.queue")
public class DedicatedProcessingProperties {
  @Builder.Default
  private Set<String> merchantCodes = new HashSet<>();
  private String partitionKey;
  private boolean l5ProcessingEnabled;
  private boolean l4ProcessingEnabled;
  private boolean l3ProcessingEnabled;

  public void setMerchantCodes(List<String> merchantCodes) {
    this.merchantCodes = new HashSet<>(merchantCodes);
  }

  public boolean isHighVolumeSeller(String merchantCode) {
    return CollectionUtils.isNotEmpty(merchantCodes) && merchantCodes.contains(merchantCode);
  }
}