package com.gdn.mta.product.service;


import java.util.List;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import org.springframework.data.domain.Page;

import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;

public interface SummaryFilterService {

  /**
   *
   * @param storeId
   * @param activated
   * @param viewable
   * @return
   * @throws Exception
   */
  FilterCountResponse getFilterCountsByStoreIdAndActivatedAndViewable(String storeId, boolean activated,
      boolean viewable) throws Exception;

  /**
   *
   * @param storeId
   * @param request
   * @param activated
   * @param viewable
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ReviewProductResponse> getReviewProductsByFilterRequestAndActivatedAndViewableFlag(String storeId,
      SummaryFilterRequest request, boolean activated, boolean viewable, int page, int size) throws Exception;

  /**
   *
   * @param storeId
   * @param request
   * @param activated
   * @param viewable
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ProductBusinessPartnerMapperResponse> getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(
      String storeId, SummaryFilterServiceRequest request, boolean activated, boolean viewable, int page, int size)
      throws Exception;

  /**
   *
   * @param storeId
   * @param request
   * @param activated
   * @param viewable
   * @return
   * @throws Exception
   */
  List<AssigneeResponse> getAssigneeListByFilterRequestAndActivatedAndViewableFlag(String storeId,
      SummaryFilterServiceRequest request, boolean activated, boolean viewable) throws Exception;

  /**
   * get history of variants by product sku and keyword
   * @param storeId
   * @param historyRequest
   * @param page
   * @param size
   * @return
   */
  Page<HistoryResponse> getProductHistoryByProductSkuAndKeyword(String storeId, HistoryRequest historyRequest, int page, int size);

  /**
   * Fetch update-history by request with pagination
   *
   * @param storeId
   * @param requestId
   * @param historyUpdateRequest
   * @param page
   * @param size
   * @return
   */
  Page<HistoryUpdateResponse> getProductUpdateHistoryByRequest(String storeId, String requestId,
    HistoryUpdateRequest historyUpdateRequest, int page, int size);
}
