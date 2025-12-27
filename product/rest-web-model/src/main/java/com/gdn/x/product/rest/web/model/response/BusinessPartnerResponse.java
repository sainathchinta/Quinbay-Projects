package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BusinessPartnerResponse extends BaseResponse {

  private static final long serialVersionUID = -7496245176197994078L;

  private String businessPartnerCode;
  private String businessPartnerName;
  private String name;
  private String businessPartnerType;
  private String merchantStatus;
  private String merchantType;
  private String linkedPartnerStore;
  private boolean internationalFlag;
  private boolean umkmFlag;
  private boolean offlineToOnlineFlag;
  private String inventoryFulfillment;
  private boolean allCategory;
  private boolean cncActivated;
  private boolean supplierFlag;
  private boolean customerFlag;
  private boolean merchantFlag;
  private String merchantDeliveryType;
  private String businessPartnerAlias;
}
