package com.gdn.mta.bulk.models;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3Response extends BaseResponse {

  private String productSku;
  private String productCode;
  private ProductType productType;
  private boolean markForDelete;
  private boolean off2OnChannelActive;
  private MasterDataProductDTO masterDataProduct;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String sizeChartCode;
  private String videoUrl;
  private String url;
  private String merchantCode;
  private MasterCatalogDTO masterCatalog;
  private Integer dangerousGoodsLevel;
  private boolean b2cActivated;
  private List<ItemCatalogDTO> itemCatalogs = new ArrayList<ItemCatalogDTO>();
}
