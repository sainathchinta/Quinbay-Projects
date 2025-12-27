package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.entity.ProductLevel3Logistics;

import com.gdn.mta.product.entity.VideoAddEditRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3UpdateRequest extends BaseRequest {

  private static final long serialVersionUID = -8611382808482810163L;

  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private boolean synchronize;
  private String productName;
  private Integer productType;
  private String categoryCode;
  private String brand;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private Integer dangerousGoodsLevel;
  private Boolean lateFulfillment;
  private Boolean online;
  private Boolean cnc;
  private boolean productEditable;
  private Boolean installationRequired;
  private String accessChannel;
  private Long version;
  private List<ProductLevel3Logistics> productLevel3LogisticsRequest;
  private PreOrderRequest preOrder;
  private Boolean b2cActivated;
  private boolean off2OnChannelActive;
  private VideoAddEditRequest videoAddEditRequest;
  private boolean sellerOmg;

  public ProductLevel3UpdateRequest(String productSku, String productCode, String businessPartnerCode,
      boolean productEditable, boolean synchronize) {
    this.productSku = productSku;
    this.productCode = productCode;
    this.businessPartnerCode = businessPartnerCode;
    this.productEditable = productEditable;
    this.synchronize = synchronize;
  }
}
