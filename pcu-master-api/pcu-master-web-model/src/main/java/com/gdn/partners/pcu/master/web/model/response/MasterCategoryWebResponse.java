package com.gdn.partners.pcu.master.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class MasterCategoryWebResponse {
  private String id;
  private String name;
  private String categoryCode;
  private Integer sequence;
  private String shortDescription;
  private byte[] description;
  private byte[] defaultDescription;
  private String state;
  private boolean display;
  private Integer logisticAdjustment;
  private boolean warranty;
  private boolean needIdentity;
  private boolean activated;
  private boolean viewable;
  private CatalogDetailResponse catalog;
  private String parentCategoryId;
  private Integer internalActivationInterval;
  private String nameEnglish;
  private byte[] descriptionEnglish;
  private long childCount;
  private boolean umkm;
  private int dangerousGoodsLevel;
  private boolean wholesalePriceConfigEnabled;
  private boolean genericTemplateEligible;
  private String documentType;
  private String oscId;
  private String oscUpdatedBy;
  private Date oscUpdatedDate;
}
