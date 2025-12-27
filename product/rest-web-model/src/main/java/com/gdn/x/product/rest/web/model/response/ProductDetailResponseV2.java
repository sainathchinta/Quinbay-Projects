package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductDetailResponseV2 extends BaseResponse {
  private static final long serialVersionUID = 2529518768864523951L;
  private String productSku;
  private String productCode;
  private ProductType productType;
  private String merchantCode;
  private boolean markForDelete;
  private MasterCatalogDTO masterCatalog;
  private boolean isSuspended;
  private boolean isTakenDown;
  private List<SalesCatalogDTO> salesCatalogs;
  private MasterDataProductV2 masterDataProduct;
}