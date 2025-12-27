package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryInfoUpdateRequest extends BaseDTORequest {

  private static final long serialVersionUID = -4163911563061086569L;

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
  private boolean activated;
  private Boolean ageLimit;
  private int dangerousGoodsLevel;
  private boolean umkm;
  private boolean genericTemplateEligible;
  private boolean wholesalePriceConfigEnabled;
  private boolean sizeChartRequired;
  private String documentType;
  private String oscId;
  private Boolean b2bExclusive;
  private Boolean halalCategory;
  private boolean bopisEligible;
}
