package com.gdn.x.productcategorybase.dto.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
public class ProductItemImageUpdateRequest extends BaseDTORequest {

  private String productCode;
  private boolean needCorrection;
  private boolean activatedBefore;
  private List<Image> copyToAllVariantImages;
  private List<ProductItemImageRequest> updateProductItemImages;
  private List<ProductItemImageRequest> newProductItemImages;
}
