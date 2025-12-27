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
@ConfigurationProperties("pickup.point")
public class PickupPointProperties {

  private Set<String> availableCampaignTypes;

  private Set<String> restrictedMerchantCodes;

  private int nearestSize;

  private boolean onePartyActivated;

  private boolean sortByInStock;

  private boolean updateStockFromInventoryEvent;

  private boolean newPickupPointSortingEnabled;

  private boolean alwaysUpdateMerchantDiscount;

  private boolean prioritizeLowestPriceEnabled;

  public void setAvailableCampaignTypes(List<String> availableCampaignTypes) {
    this.availableCampaignTypes = new HashSet<>(availableCampaignTypes);
  }

  public void setRestrictedMerchantCodes(List<String> restrictedMerchantCodes) {
    this.restrictedMerchantCodes = new HashSet<>(restrictedMerchantCodes);
  }

}
