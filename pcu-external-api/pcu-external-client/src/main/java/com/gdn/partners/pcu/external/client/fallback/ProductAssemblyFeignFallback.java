package com.gdn.partners.pcu.external.client.fallback;

import java.util.List;

import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.client.feign.ProductAssemblyFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.AssemblyDisAssemblyListingRequest;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.request.TransferRequest;
import com.gdn.partners.pcu.external.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBundleRecipeEditableResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormResponse;

@Component
public class ProductAssemblyFeignFallback implements ProductAssemblyFeign {

  @Override
  public GdnRestListResponse<MasterWarehouseListWebResponse> getWarehouseCodeAndFulfillmentCenter(String page,
      String limit) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<RequestFormResponse> getListingResponse(int page, int size,
      AssemblyDisAssemblyListingRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<HistoryWebResponse> getHistory(int page, int size, String sortOrder, String requestFormNumber, String merchantCode) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ProductBundleRecipeEditableResponse> getBundleRecipeEditableInfoByItemCodes(
      List<String> itemCodes) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse transferRequest(TransferRequest transferRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse assemblyDisassemblyRequest(String type,
      SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse cancelOrRetryRequestForm(String requestFormName, String type, String merchantCode, String requestId) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }
}
