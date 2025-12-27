package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class BrandAuthWipDetailResponse extends BaseResponse {
  private String status;
  private String brandName;
  private String iprRegistrationNumber;
  private Date authStartDate;
  private Date authExpireDate;
  private String sellerName;
  private String reasons;
  private Date currentAuthStartDate;
  private Date currentAuthExpireDate;
  private List<String> documentList = new ArrayList<>();
  private String brandCode;
  private String sellerCode;
}