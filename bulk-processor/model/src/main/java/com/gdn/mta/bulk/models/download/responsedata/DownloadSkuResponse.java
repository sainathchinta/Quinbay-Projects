package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class DownloadSkuResponse extends BaseResponse {

  private static final long serialVersionUID = 5444032244501259174L;

  private String itemSku;
  private String pickupPointCode;
  private String pickupPointName;
  private String itemSkuName;
  private String storeName;
  private String c1CategoryName;
  private String cnCategoryName;
  private String brandName;
  private String productType;
  private float minMargin;
  private float maxMargin;
  private double listPrice;
  private double offerPrice;
  private Double bmlPrice;
  private Float cogs;
  private Boolean vatInclusive;
  private Float rebate;
  private SkuRebateResponse rebateBreakdown;
  private boolean rebateOverwrittenByOfficer;
  private String bml;
  private Long totalOrders;
  private Long totalTPV;
  private Long l2Stock;
  private Long l5Stock;
  private Long l2DaysOfInventory;
  private Long l2TargetDaysOfInventory;
  private CampaignPriceInfo campaignPriceInfo;
  private String expiryDate;
  private Long expiringQuantity;
}
