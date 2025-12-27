package com.gda.mta.product.dto;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryCountResponse extends BaseResponse {
  private static final long serialVersionUID = 117582036171462821L;
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
