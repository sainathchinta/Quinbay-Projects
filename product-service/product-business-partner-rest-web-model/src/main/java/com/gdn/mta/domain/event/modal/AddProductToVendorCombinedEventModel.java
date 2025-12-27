package com.gdn.mta.domain.event.modal;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddProductToVendorCombinedEventModel extends GdnBaseDomainEventModel {
  private ScreeningProductApprovalEvent screeningProductApprovalEvent;
  private AddEditedProductToPDTEvent addEditedProductToPDTEvent;
  private AddRevisedProductToPDTEvent addRevisedProductToPDTEvent;
  private PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel;
  private AutoApprovalTypeRequestModel autoApprovalTypeRequestModel;
}
