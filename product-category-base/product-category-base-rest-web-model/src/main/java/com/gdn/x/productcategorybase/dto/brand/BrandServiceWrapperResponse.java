package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class BrandServiceWrapperResponse {

  private BrandWip brandWip;
  private Brand brand;
  private boolean unDeleteBrand = false;
}
