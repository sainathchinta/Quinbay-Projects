package com.gdn.partners.pcu.external.service;


import org.springframework.data.domain.Pageable;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.web.model.request.AssemblyDisAssemblyListingRequest;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormWebResponse;

public interface ProductAssemblyService {

  /**
   * API to fetch the warehouse code and name list
   *
   * @return MasterWarehouseListWebResponse List
   */
  GdnRestListResponse<MasterWarehouseListWebResponse> getMasterWarehouseListResponse();

  /**
   * GET requestforms for listing
   * @param page
   * @param size
   * @param request
   * @return RequestFormWebResponse
   */
  GdnRestListResponse<RequestFormWebResponse> getRequestFormsListingResponse(int page, int size, String requestId,
      AssemblyDisAssemblyListingRequest request);

  /**
   *
   * @param requestFormNumber
   * @param page
   * @param size
   * @param requestId
   * @return
   */
  GdnRestListResponse<HistoryWebResponse> getRequestHistory(String requestFormNumber, int page, int size, String sortOrder, String requestId, String merchantCode);

  /**
   * Create Assembly/DisAssembly/Transfer Requests
   *
   * @param type
   * @param simpleListAssemblyDisassemblyRequest
   */
  void createAssemblyDisAssemblyAndTransferRequests(String businessPartnerCode, String type,
      SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest);

  /**
   * Cancel/Retry Request Form
   * @param requestFormNumber
   * @param type
   * @param requestId
   * @return
   */
  GdnBaseRestResponse cancelOrRetry(String requestFormNumber, String type, String merchantCode, String requestId);
}
