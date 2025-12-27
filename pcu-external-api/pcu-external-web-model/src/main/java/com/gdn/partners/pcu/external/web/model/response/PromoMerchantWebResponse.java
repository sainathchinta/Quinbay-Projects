package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantWebResponse extends BaseResponse{
  private String promoAdjustmentCode;
  private String promoAdjustmentName;
  private String merchantCode;
  private String promoAdjustmentStatus;
  private String promoAdjustmentType;
  private String adjustmentQuotaType;
  private Date startDate;
  private Date endDate;
}
