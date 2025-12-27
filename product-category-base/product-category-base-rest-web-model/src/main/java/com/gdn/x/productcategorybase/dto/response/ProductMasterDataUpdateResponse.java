package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.Builder;
import lombok.Data;

import java.util.Map;

@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMasterDataUpdateResponse extends BaseDTOResponse {
  private Map<String, Map<String, String>> productImagesErrorMap;
  private ErrorMessage errorMessage;
}