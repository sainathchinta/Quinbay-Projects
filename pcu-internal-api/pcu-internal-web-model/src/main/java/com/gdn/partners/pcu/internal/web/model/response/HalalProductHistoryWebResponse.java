package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class HalalProductHistoryWebResponse {
  private String productSku;
  private String activity;
  private String previousValue;
  private String currentValue;
  private Date updatedDate;
  private String updatedBy;
  private Date createdDate;
  private String createdBy;
}
