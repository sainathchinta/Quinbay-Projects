package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Builder
@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductEditValidationDTO {
  ProductL3UpdateRequest productL3UpdateRequest;
  EditProductResponse editProductResponse;
  ProductL3Response productL3Response;
  ProductLevel3 productLevel3;
  boolean L5ValidationFailed;
  boolean needCorrection;
}
