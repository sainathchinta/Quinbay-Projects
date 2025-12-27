package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BlimartSubscriptionChangeRequest extends ProductBaseDomainEventModel {

  private String itemSku;
  private boolean subscribable;
  private Set<String> preferredSubscriptionType;
  private String pickupPointCode;

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BlimartSubscriptionChangeRequest{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", subscribable=").append(subscribable);
    sb.append(", preferredSubscriptionType=").append(preferredSubscriptionType);
    sb.append(", pickupPointCode=").append(pickupPointCode);
    sb.append('}');
    return sb.toString();
  }

  public BlimartSubscriptionChangeRequest(String itemSku, boolean subscribable, Set<String> preferredSubscriptionType) {
    this.itemSku = itemSku;
    this.subscribable = subscribable;
    this.preferredSubscriptionType = preferredSubscriptionType;
  }
}
