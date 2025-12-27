package com.gdn.x.mta.distributiontask.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class SellerAnalyticsResponse extends BaseResponse {
  private static final long serialVersionUID = 8249282152321812684L;
  private String sellerCode;
  private String sellerName;
  private String sellerBadge;
  private String commissionType;
  private boolean officialSeller;
  private Date latestFraudDetectedDate;
  private int rejectedProductCount;
  private int piratedProductCount;
  private int expiredProductCount;
  private int illegalProductCount;
  private int prohibitedProductCount;
  private int counterfeitProductCount;
  private int unauthorizedSellerProductCount;
  private int unauthorizedResellerProductCount;
  private int suspiciousSkuProductCount;
  private int copyRightInfringementProductCount;
  private int trademarkInfringementProductCount;
  private int designRightInfringementProductCount;
  private int repackageProductCount;
  private int patentProductCount;
}
