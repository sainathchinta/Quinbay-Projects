package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingV2WebResponse;
import org.springframework.data.domain.Page;

import java.util.List;

public interface ProductWrapperV2Service {

  /**
   * Quick edit of products on PLP
   *
   * @param productSku
   * @param quickEditRequests
   */
  void updateItemListing(String productSku, List<QuickEditV2WebRequest> quickEditRequests)
    throws Exception;

  /**
   * L3 listing of products for PLP
   *
   * @param request
   * @param page
   * @param size
   * @param onlyDefaultViewConfig
   * @return
   */
  Page<ProductLevel3ListingV2WebResponse> getProductL3Listing(ProductSummaryV2WebRequest request,
    Integer page, Integer size, boolean onlyDefaultViewConfig);
}
