package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthorisationWipListResponse extends BaseResponse {
  private String brandCode;
  private String brandName;
  private String authorisationStatus;
  private String iprRegistrationNumber;
  private String documentLink;
  private String reasons;
  private Date authStartDate;
  private Date authExpireDate;
}
