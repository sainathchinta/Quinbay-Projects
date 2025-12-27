package com.gdn.x.mta.distributiontask.rest.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class BoostedProductFilterRequest implements Serializable {
  private static final long serialVersionUID = 7107868130952665001L;
  private String timeFilterWebType;
  private Boolean contentPending;
  private Boolean imagePending;
  private String businessPartnerCode;
  private Boolean postLive;
  private String faultyType;
  private Boolean brandPending;
  private Boolean edited;
  private Boolean revised;
  private Boolean restrictedKeyword;
}
