package com.gdn.partners.pcu.master.model.request;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryInfoUpdateServiceRequest {

  private String id;
  private String storeId;
  private String name;
  private String nameEnglish;
  private String categoryCode;
  private String parentCategoryId;
  private byte[] defaultDescription;
  private byte[] descriptionEnglish;
  private Integer logisticAdjustment;
  private Integer internalActivationInterval;
  private boolean deliveredByMerchant;
  private boolean directFlight;
  private boolean specialHandling;
  private boolean needIdentity;
  private boolean warranty = false;
  private boolean display;
  private Integer sequence;
  private Date updatedDate;
  private String updatedBy;
  private Boolean ageLimit;
  private boolean umkm;
  private int dangerousGoodsLevel;
  private boolean wholesalePriceConfigEnabled;
  private boolean genericTemplateEligible;
  private String documentType;
  private boolean sizeChartRequired;
  private String oscId;
  private Boolean b2bExclusive;
  private Boolean halalCategory;
  private boolean bopisEligible;
}
