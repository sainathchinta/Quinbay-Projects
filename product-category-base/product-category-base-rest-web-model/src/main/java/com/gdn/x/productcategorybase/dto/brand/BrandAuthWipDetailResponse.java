package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class BrandAuthWipDetailResponse extends BaseResponse {
  private String status;
  private String brandName;
  private String iprRegistrationNumber;
  private Date authStartDate;
  private Date authExpireDate;
  private Date currentAuthStartDate;
  private Date currentAuthExpireDate;
  private String reasons;
  private List<String> documentList = new ArrayList<>();
  private String brandCode;
  private String sellerCode;
}
