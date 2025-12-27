package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3UpdateWebRequest {
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
  private boolean productEditable;
  private Boolean installationRequired;
  private String accessChannel;
  private Long version;
  private PreOrderWebRequest preOrder;
  private List<ProductLevel3LogisticsWebRequest> logistics;
}
