package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class BasicProductResponse extends BaseResponse {
  private static final long serialVersionUID = -4623480772250833406L;

  private String productSku;
  private String productCode;
  private String productName;
  private String merchantCode;
  private boolean forceReview;
  private boolean archived;
  private boolean takenDown;
  private boolean markForDelete;
  private boolean bundleProduct;
  private String categoryCode;
  private ProductType productType;
  private boolean b2cActivated;
  private boolean off2OnChannelActive;
  private boolean sharedProduct;
  private boolean syncProduct;
  private PreOrderDTO preOrder;
}
