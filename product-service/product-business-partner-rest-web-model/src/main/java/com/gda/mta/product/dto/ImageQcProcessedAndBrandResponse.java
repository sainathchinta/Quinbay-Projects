package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
@JsonInclude()
public class ImageQcProcessedAndBrandResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 6934308530468044721L;
  private String brandCode;
  private String brandApprovalStatus;
  private Integer productType;
  private ImageQcProcessedResponse imageQcProcessedResponse;
}
