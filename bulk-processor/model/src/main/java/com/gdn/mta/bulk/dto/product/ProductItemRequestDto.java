package com.gdn.mta.bulk.dto.product;

import java.util.List;
import java.util.Map;

import com.gdn.x.productcategorybase.dto.response.AttributeResponse;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class ProductItemRequestDto {

  private List<AttributeResponse> nonDefiningItemAttributeDetails;
  private String productCode;
  private Map<String, AttributeResponse> defAttrMap;
  private Map<String, AttributeResponse> nonDefAttrMap;
  private Map<String, List<AllowedValueDtoResponse>> allowedValue;
}
