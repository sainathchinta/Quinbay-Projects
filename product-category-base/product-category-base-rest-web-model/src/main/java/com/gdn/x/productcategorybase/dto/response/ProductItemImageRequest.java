package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.Image;

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
public class ProductItemImageRequest extends BaseDTORequest {

  private String skuCode;
  private List<Image> itemImages;


}
