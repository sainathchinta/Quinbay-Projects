package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMasterDataItemResponse extends BaseDTOResponse {
  private String catalogType;
  private String category;
  private String skuCode;
  private String description;
  private String uom;
  private double weight;
  private double width;
  private double height;
  private double length;
  private double shippingWeight;
  private String generatedItemName;
  private Integer dangerousGoodsLevel;
  private String productCode;
  private String upcCode;
  private boolean vatApplicable;
  private boolean activated;
  private String merchantCode;
  private String brand;
  private String categoryName;
  private boolean productMarkForDelete;
  private boolean productItemMarkForDelete;
}
