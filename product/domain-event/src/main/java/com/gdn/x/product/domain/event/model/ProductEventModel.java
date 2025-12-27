package com.gdn.x.product.domain.event.model;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.ProductScoreVo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductEventModel extends BaseResponse {
  private String productSku;
  private String merchantCode;
  private String productCode;
  private ProductType productType;
  private String productCatentryId;
  private ProductScoreVo productScore;
  private List<SalesCatalogModel> salesCatalogs;
  private boolean tradingProduct;
  private boolean isSuspended;
  private String catalogCode;
  private String categoryCode;
  private String brand;
  private String productName;
  private boolean isSynchronized;
  private boolean isArchived;
  private boolean off2OnChannelActive;
  private Boolean preOrder;
  private boolean freeSample;
  private boolean cncActivated;
  private int distributionStatus;
  private Set<String> pickupPointCodes;
  private boolean markForDelete;
  private boolean fbbActivated;
  private boolean b2cActivated;
  private boolean b2bActivated;
  private int curationStatus;
  private boolean bundleProduct;
  private String sizeChartCode;
}
