package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.enums.ErrorMessage;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMasterDataUpdateResponse extends BaseDTOResponse {
  private Map<String, Map<String, String>> productImagesErrorMap = new HashMap<>();
  private ErrorMessage errorMessage;
}