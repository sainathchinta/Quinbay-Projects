package com.gdn.partners.pcu.internal.web.model.response;

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
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class CategoryConfigurationHistoryWebResponse {

  String categoryCode;
  String categoryName;
  String activity;
  String activityIn;
  String oldValue;
  String newValue;
  Date updatedDate;
  String updatedBy;
  Date createdDate;
  String createdBy;
}
