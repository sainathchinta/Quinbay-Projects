package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.HashMap;
import java.util.Map;

@Builder
@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemImageHistoryDTO {
  ProductItemImageUpdateRequest productItemImageUpdateRequest;
  Map<String, String> itemSkuItemNameMap = new HashMap<>();
  Map<String, String> skuCodeItemSkuMap = new HashMap<>();

}
