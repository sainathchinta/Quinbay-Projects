package com.gdn.partners.pbp.model.productlevel3;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3Inventory {

  private String webItemSku;
  private String webMerchantCode;
  private Integer webAvailable;
  private Integer initialPreOrderQuota;
  private Integer actualAvailableStock;
  private Integer webReserved;
  private Integer webMinAlert;
  private boolean webSyncStock;
  private String webPickupPointCode;
  private String warehouseItemSku;
  private String warehouseMerchantCode;
  private Integer warehouseAvailable;
  private Integer warehouseReserved;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;
  private String productSku;
  private boolean fbbPP;
  private Date preOrderDate;
  private boolean distributionPickupPoint;

  public int getNullSafeInitialPreOrderQuota() {
    return Objects.requireNonNullElse(initialPreOrderQuota, 0);
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
      .append("webItemSku", webItemSku)
      .append("webMerchantCode", webMerchantCode)
      .append("webAvailable", webAvailable)
      .append("webReserved", webReserved)
      .append("webMinAlert", webMinAlert)
      .append("webSyncStock", webSyncStock)
      .append("webPickupPointCode", webPickupPointCode)
      .append("warehouseItemSku", warehouseItemSku)
      .append("warehouseMerchantCode", warehouseMerchantCode)
      .append("warehouseAvailable", warehouseAvailable)
      .append("warehouseReserved", warehouseReserved)
      .append("nonDistributionAvailable", nonDistributionAvailable)
      .append("nonDistributionReserved", nonDistributionReserved)
      .append("actualAvailableStock", actualAvailableStock)
      .append("initialPreOrderQuota", initialPreOrderQuota)
      .toString();
  }

}
