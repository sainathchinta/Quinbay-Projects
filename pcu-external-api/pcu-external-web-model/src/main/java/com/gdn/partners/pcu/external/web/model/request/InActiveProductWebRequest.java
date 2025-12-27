package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

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
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class InActiveProductWebRequest {

  private String businessPartnerCode;
  private List<String> categoryCodes;
  private List<String> pickupPointCodes;
  private String searchKey;
  private String sortBy;
  private String orderBy;
}
