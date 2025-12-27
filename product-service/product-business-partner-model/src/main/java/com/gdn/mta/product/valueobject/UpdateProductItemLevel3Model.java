package com.gdn.mta.product.valueobject;

import com.gdn.mta.product.commons.annotation.LogAuditUpdateProduct;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UpdateProductItemLevel3Model {
  @LogAuditUpdateProduct(UpdateProductActivity.PRODUCT_NAME)
  private String productName;

  @LogAuditUpdateProduct(UpdateProductActivity.PRODUCT_DESC)
  private String description;

  @LogAuditUpdateProduct(UpdateProductActivity.STOCK_VALUE)
  private Integer availableStockLevel2;

  @LogAuditUpdateProduct(UpdateProductActivity.SYNC_STOCK)
  private Boolean synchronizeStock;

  @LogAuditUpdateProduct(UpdateProductActivity.SYNC_CONTENT)
  private Boolean synchronizeContent;
  
  @LogAuditUpdateProduct(UpdateProductActivity.OFFLINE_TO_ONLINE)
  private Boolean off2OnActiveFlag;

  @LogAuditUpdateProduct(UpdateProductActivity.MERCHANT_SKU)
  private String merchantSku;

  @LogAuditUpdateProduct(UpdateProductActivity.NORMAL_PRICE)
  private Double price;

  @LogAuditUpdateProduct(UpdateProductActivity.SELLING_PRICE)
  private Double salePrice;

  @LogAuditUpdateProduct(UpdateProductActivity.DISPLAYABLE)
  private Boolean displayable;

  @LogAuditUpdateProduct(UpdateProductActivity.BUYABLE)
  private Boolean buyable;

  @LogAuditUpdateProduct(UpdateProductActivity.CNC_DISPLAYABLE)
  private Boolean cncDisplayable;

  @LogAuditUpdateProduct(UpdateProductActivity.CNC_BUYABLE)
  private Boolean cncBuyable;
  
  @LogAuditUpdateProduct(UpdateProductActivity.LATE_FULFILLMENT)
  private Boolean lateFulfillment;

  @LogAuditUpdateProduct(UpdateProductActivity.PRODUCT_TYPE)
  private String productType;

  @LogAuditUpdateProduct(UpdateProductActivity.WIDTH)
  private Double width;

  @LogAuditUpdateProduct(UpdateProductActivity.HEIGHT)
  private Double height;

  @LogAuditUpdateProduct(UpdateProductActivity.LENGTH)
  private Double length;

  @LogAuditUpdateProduct(UpdateProductActivity.WEIGHT)
  private Double weight;

  @LogAuditUpdateProduct(UpdateProductActivity.DG_LEVEL)
  private Integer dangerousGoodsLevel;

  @LogAuditUpdateProduct(UpdateProductActivity.PICK_POINT_CODE)
  private String pickupPointCode;

  @LogAuditUpdateProduct(UpdateProductActivity.ATTRIBUTES)
  private String[] attributesMap;

  @LogAuditUpdateProduct(UpdateProductActivity.INSTALLATION_FLAG)
  private String installationFlag;

  @LogAuditUpdateProduct(UpdateProductActivity.URL_VIDEO)
  private String urlVideo;

  @LogAuditUpdateProduct(UpdateProductActivity.USP)
  private String usp;

  @LogAuditUpdateProduct(UpdateProductActivity.MINIMUM_STOCK)
  private String minimumStock;

  @LogAuditUpdateProduct(UpdateProductActivity.WHOLE_PRICE_FLAG)
  private String wholesalePriceActivated;

  @LogAuditUpdateProduct(UpdateProductActivity.UPDATE_WHOLE_SALE_RULES)
  private String wholesaleRules;

  @LogAuditUpdateProduct(UpdateProductActivity.UPC_CODE)
  private String upcCode;

  @LogAuditUpdateProduct(UpdateProductActivity.LOGISTIC)
  private String logistic;

  @LogAuditUpdateProduct(UpdateProductActivity.PRE_ORDER)
  private Object preOrder;

  @LogAuditUpdateProduct(UpdateProductActivity.FREE_SAMPLE)
  private Boolean freeSample;

  @LogAuditUpdateProduct(UpdateProductActivity.CNC)
  private Boolean cnc;

  @LogAuditUpdateProduct(UpdateProductActivity.FBB_ACTIVATED)
  private Boolean fbbActivated;

  @LogAuditUpdateProduct(UpdateProductActivity.FBB_DEACTIVATED)
  private Boolean fbbDeactivated;

  @LogAuditUpdateProduct(UpdateProductActivity.SIZE_CHART_CHANGE)
  private String sizeChartCode;
  // sizeChartChanged to support backward compatibility from App , not for history
  private boolean sizeChartChanged;
}
