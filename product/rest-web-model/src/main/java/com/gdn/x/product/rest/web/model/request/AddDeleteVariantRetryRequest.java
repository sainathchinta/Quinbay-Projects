package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AddDeleteVariantRetryRequest {
  private String productCode;
  private String productSku;
  private List<ItemActivationRequest> itemActivationRequestList;
}
