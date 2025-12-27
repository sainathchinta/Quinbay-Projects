package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by parvej on 03/02/2020 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
public class InProcessProductWebRequest {

  private String criteria;
  private List<String> categoryCodes;
  private String searchKey;
  private String businessPartnerCode;
  private String sortBy;
  private String orderBy;
}
