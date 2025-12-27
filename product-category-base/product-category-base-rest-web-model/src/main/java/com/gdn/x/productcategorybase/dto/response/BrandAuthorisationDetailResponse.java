package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthorisationDetailResponse extends BaseResponse {
  private static final long serialVersionUID = -5849646461142700552L;
  private String brandName;
  private String sellerId;
  private Date authStartDate;
  private Date authExpireDate;
  private BrandAuthorisationStatus authorisationStatus;
  private String sellerName;
}
