package com.gdn.mta.bulk.repository;

import org.springframework.data.domain.Pageable;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;

public interface ProductLevel1Repository {
  
   GeneratedProductImagesPathResponse replaceProductImages(String requestId, String username,
      String businessPartnerCode, ReplaceProductImagesRequest productImageRequest) throws Exception;

   /**
    * Get Products which are in reviewed state
    * @param username
    * @param requestId
    * @param summaryFilterRequest
    * @return
    * @throws Exception
    */
   GdnRestListResponse<ReviewProductResponse> getReviewProducts(String username,
       String requestId, SummaryFilterRequest summaryFilterRequest, Pageable pageable) throws Exception;
}
