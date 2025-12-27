package com.gdn.mta.domain.event.modal;

import com.gdn.mta.product.entity.ProductCollection;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class StuckProductEventPublishDto {
  private AddRevisedProductToPDTEvent addRevisedProductToPDTEvent;
  private ProductCollection productCollection;
  private ProductCollection editedPublishProductCollection;
  private ProductCollection vendorApprovalProductCollection;
  private boolean trustedSeller;
}
