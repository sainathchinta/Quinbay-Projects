package com.gdn.partners.pcu.internal.web.model.response;

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
public class ProductBusinessPartnerMapperWebResponse {

  private static final long serialVersionUID = -6280258921917397350L;

  private String businessPartnerCode;
  private String businessPartnerName;
}
