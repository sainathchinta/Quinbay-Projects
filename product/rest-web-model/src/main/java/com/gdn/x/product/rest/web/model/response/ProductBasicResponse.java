package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductBasicResponse extends BaseResponse {

  private static final long serialVersionUID = 4728608509634311649L;
  private boolean productExists;
  private String productSku;
  private String productName;
  private boolean suspended;
  private boolean archived;
  private boolean markForDelete;
  private boolean tradingProduct;
  private boolean bundleProduct;
  private String categoryCode;
  private ProductType productType;
  private List<String> salesCategoryCodes = new ArrayList<>();
  private List<CategoryDataResponse> salesCategoryHierarchy = new ArrayList<>();
  private List<CategoryHierarchyDataResponse> salesCategoryHierarchyV2 = new ArrayList<>();
  private PreOrderDTO preOrder;
}
