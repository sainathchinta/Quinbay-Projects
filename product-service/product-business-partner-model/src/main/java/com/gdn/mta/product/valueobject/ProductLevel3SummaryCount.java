package com.gdn.mta.product.valueobject;

import java.util.Map;

import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel3SummaryCount {
  private String businessPartnerCode;
  private Map<ProductLevel3InventoryCriteria, Long> stockConditionCounts;
  private Long totalCounts;

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public Map<ProductLevel3InventoryCriteria, Long> getStockConditionCounts() {
    return stockConditionCounts;
  }

  public void setStockConditionCounts(Map<ProductLevel3InventoryCriteria, Long> stockConditionCounts) {
    this.stockConditionCounts = stockConditionCounts;
  }

  public Long getTotalCounts() {
    return totalCounts;
  }

  public void setTotalCounts(Long totalCounts) {
    this.totalCounts = totalCounts;
  }

}
