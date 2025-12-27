package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;
import java.util.List;

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
public class BrandAuthFilterWebResponse extends BaseResponse {

  private String brandCode;
  private String brandName;
  private String sellerCode;
  private Date authStartDate;
  private Date authEndDate;
  private List<String> documentLinks;
  private String status;
  private String sellerName;
  private String reasons;
}
