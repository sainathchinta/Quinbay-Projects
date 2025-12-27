package com.gdn.partners.pcu.internal.web.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
public class CountWebRequest implements Serializable {

  private static final long serialVersionUID = 7116614406479905890L;

  private String keyword;
  private String source;
  private String categoryCode;
  private String businessPartnerCode;
  private String status;
}
