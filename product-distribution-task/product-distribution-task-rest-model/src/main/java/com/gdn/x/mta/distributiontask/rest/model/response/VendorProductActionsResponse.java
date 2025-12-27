package com.gdn.x.mta.distributiontask.rest.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@JsonInclude
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class VendorProductActionsResponse extends BaseDTOResponse {
  private static final long serialVersionUID = 4803224072023219386L;

  private List<String> productCode;
  private boolean updateSuccess;
  private String reasonOfFailure;


}
