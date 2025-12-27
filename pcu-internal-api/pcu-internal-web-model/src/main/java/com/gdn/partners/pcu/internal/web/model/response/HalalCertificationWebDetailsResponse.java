package com.gdn.partners.pcu.internal.web.model.response;


import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
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
public class HalalCertificationWebDetailsResponse extends BaseResponse {

  private static final long serialVersionUID = -5798993047894446685L;

  private String certificationNumber;
  private Date issuedDate;
  private Date expirationDate;
  private String productName;
}
