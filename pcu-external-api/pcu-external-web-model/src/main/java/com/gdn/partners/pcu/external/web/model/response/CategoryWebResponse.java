package com.gdn.partners.pcu.external.web.model.response;


import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryWebResponse {

  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;
  private String name;
  private String nameEnglish;
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
  private String parentCategoryId;
  private Integer internalActivationInterval;
  private long childCount;
  private byte[] descriptionEnglish;
}
