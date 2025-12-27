package com.gdn.x.product.rest.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
@Data
@ToString
public class ProductEditRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private ProductRequest productRequest;
  private String productSku;
  private boolean forceReview;
  private List<OmniChannelSkuUpdateRequest> omniChannelSkuUpdateRequests = new ArrayList<>();
  private List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest;
  private boolean contentChanged;
  private Boolean latefulfillment;
}
