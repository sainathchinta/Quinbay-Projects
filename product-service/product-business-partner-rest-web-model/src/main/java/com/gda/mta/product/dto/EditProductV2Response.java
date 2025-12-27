package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.enums.ApiErrorCode;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditProductV2Response extends BaseResponse {
  private static final long serialVersionUID = -2431751171822192833L;
  private boolean productReview;
  private String reviewType;
  private ApiErrorCode apiErrorCode;
  private List<VariantsErrorListResponse> variantsErrorList = new ArrayList<>();
}



