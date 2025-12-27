package com.gdn.partners.pcu.master.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class OriginalSalesCategoryWebRequest {

  private String oscCode;
  private String oscShortText;
  private String oscLongText;
  private boolean activated = true;
}
