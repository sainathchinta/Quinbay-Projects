package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBrandUpdateRequest implements Serializable {
  private static final long serialVersionUID = -593831264333380871L;
  private String productCode;
  private String oldBrandCode;
  private String newBrandCode;
}
