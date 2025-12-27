package com.gdn.partners.pcu.master.web.model.response;

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
public class RestrictedKeywordHistoryWebResponse {
  private String keywordId;
  private String activity;
  private String oldValue;
  private String newValue;
  private Date updatedDate;
  private String updatedBy;
  private Date createdDate;
  private String createdBy;
}
