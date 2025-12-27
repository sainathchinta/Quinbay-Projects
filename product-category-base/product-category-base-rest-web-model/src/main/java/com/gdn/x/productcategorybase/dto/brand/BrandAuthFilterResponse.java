package com.gdn.x.productcategorybase.dto.brand;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthFilterResponse extends BaseResponse {

  private String brandCode;
  private String brandName;
  private String sellerCode;
  private Date authStartDate;
  private Date authEndDate;
  private List<String> documentLinks;
  private String status;
  private String reasons;

}
