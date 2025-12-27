package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3UpdateSyncStockBusinessPartnerRequest extends BaseResponse {
  private static final long serialVersionUID = -4205769152605054377L;

  private String businessPartnerCode;
  private Boolean syncStock;

  public String getBusinessPartnerCode() {
    return this.businessPartnerCode;
  }

  public Boolean getSyncStock() {
    return this.syncStock;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setSyncStock(Boolean syncStock) {
    this.syncStock = syncStock;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductLevel3UpdateSyncStockBusinessPartnerRequest [businessPartnerCode=%s, syncStock=%s]",
        this.businessPartnerCode, this.syncStock);
  }
}
