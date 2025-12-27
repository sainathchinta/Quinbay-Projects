package com.gda.mta.product.dto;

import java.io.Serializable;

import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.enums.ApiErrorCode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductLevel3DTO implements Serializable {

  private static final long serialVersionUID = 3421559253080145411L;

  private ProductLevel3 productLevel3;
  private ApiErrorCode apiErrorCode;
}
