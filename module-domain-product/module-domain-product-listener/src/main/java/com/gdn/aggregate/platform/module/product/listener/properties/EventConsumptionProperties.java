package com.gdn.aggregate.platform.module.product.listener.properties;

import java.util.List;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Configuration
@ConfigurationProperties("module.domain.product")
public class EventConsumptionProperties {
  //properties for item data change event
  private boolean itemDataChangeV2EventEnabled;
  private List<String> itemDataChangeV2EventBlackListedSellers;
  private List<String> itemDataChangeEventBlackListedSellers;

  //properties for item pickup point data change event
  private boolean itemPickupPointDataChangeV2EventEnabled;
  private List<String> itemPickupPointDataChangeV2EventBlackListedSellers;
  private List<String> itemPickupPointDataChangeBlackListedSellers;

  //properties for adjustment product event
  private boolean adjustmentProductV2EventEnabled;
  private List<String> adjustmentProductV2EventBlackListedSellers;
  private List<String> adjustmentProductEventBlackListedSellers;

  private List<String> campaignProductPublishedEventBlackListedSellers;
  private List<String> promotionAdjustmentProductSaveEventBlackListedSellers;
  private List<String> flashSaleAdjustmentProductV2EventBlackListedSellers;
}
