package com.gdn.x.mta.distributiontask.rest.model.response;

import com.gdn.partners.pdt.dto.configuration.distribution.ApproveProductResponseDto;
import com.gdn.x.mta.distributiontask.model.Product;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class QuickApprovalResponse {
  private VendorQuickApprovalResponse vendorQuickApprovalResponse;
  private Product product;
  private ApproveProductResponseDto approveProductResponseDto;

  public QuickApprovalResponse(VendorQuickApprovalResponse vendorQuickApprovalResponse,
    Product existingProduct) {
    this.vendorQuickApprovalResponse = vendorQuickApprovalResponse;
    this.product = existingProduct;
  }
}
