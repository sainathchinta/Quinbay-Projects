package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class NeedCorrectionProductActivationRequest implements Serializable {

  private static final long serialVersionUID = -5703352454787557769L;

  private String productSku;
  private List<NeedCorrectionItemActivationRequest> itemRequest;
  private PreOrderDTO preOrder;
  private ProductType productType;
  private boolean freeSample;
  private boolean off2OnChannelActive;
  private boolean online;
  private boolean cncActivated;
  private boolean fbbActivated;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private boolean bundleProduct;
  private Boolean dimensionsMissing;
  private String sizeChartCode;

  public NeedCorrectionProductActivationRequest(String productSku,
      List<NeedCorrectionItemActivationRequest> itemRequest, PreOrderDTO preOrder, ProductType productType,
      boolean freeSample, boolean off2OnChannelActive, boolean online, boolean cncActivated) {
    this.productSku = productSku;
    this.itemRequest = itemRequest;
    this.preOrder = preOrder;
    this.productType = productType;
    this.freeSample = freeSample;
    this.off2OnChannelActive = off2OnChannelActive;
    this.online = online;
    this.cncActivated = cncActivated;
  }
}
