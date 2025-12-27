package com.gdn.partners.product.analytics.properties;

import java.util.concurrent.TimeUnit;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ConfigurationProperties("big-query-process-lock")
public class SubmitBigQueryProcessLockProperties {

  private long lockTime;
  private TimeUnit lockTimeUnit;
}
