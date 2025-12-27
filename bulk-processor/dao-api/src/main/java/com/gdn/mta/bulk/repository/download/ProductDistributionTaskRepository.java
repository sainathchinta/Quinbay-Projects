package com.gdn.mta.bulk.repository.download;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.IprActionRequest;
import com.gdn.mta.bulk.models.download.IPRProductListRequest;
import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;

import java.util.List;

public interface ProductDistributionTaskRepository {

  Page<DistributionProductResponse> getProductsForVendor(String requestId, String username,
      Pageable pageable, ProductListRequest request) throws Exception;

  /**
   * Get vendor filtered products
   * @param username
   * @param requestId
   * @param pageable
   * @param request
   * @return
   * @throws Exception
   */
  GdnRestListResponse<DistributionProductResponse> getVendorFilteredProducts (String username,
      String requestId, Pageable pageable, FilterSummaryRequest request) throws Exception;

  /**
   * PDT call to assign products in batches
   *
   * @param request
   * @return
   * @throws Exception
   */
  BulkVendorProductActionsResponse bulkVendorProductActions(BulkVendorProductActionsRequest request) throws Exception;

  /**
   * PDT call to Fetch Product Codes for Auto Assignment
   *
   * @param requestId
   * @param username
   * @param page
   * @param size
   * @param boostedProductFilterRequest
   * @return
   * @throws Exception
   */
  GdnRestListResponse<ProductCodeResponse> fetchProductsForAutoAssignment(String requestId,
    String username, Integer page, Integer size,
    BoostedProductFilterRequest boostedProductFilterRequest) throws Exception;

  /**
   * Reject product for bulk rejections
   *
   * @param storeId                    10001
   * @param username                   username
   * @param vendorCode                 vendorCode
   * @param rejectProductVendorRequest request
   * @return
   */
  String vendorRejection(String storeId, String username, String vendorCode,
    RejectProductVendorRequest rejectProductVendorRequest) throws ApplicationException;

  /**
   * Quick approval of product from vendor
   * @param storeId 10001
   * @param userName username
   * @param vendorQuickApprovalRequest request
   * @return
   */
  GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApproval(String storeId,
    String userName, VendorQuickApprovalRequest vendorQuickApprovalRequest);

  /**
   *
   * @param storeId
   * @param userName
   * @param iprActionRequest
   * @return
   */
  GdnBaseRestResponse performIprAction(String storeId,
      String userName, IprActionRequest iprActionRequest);

  /**
   *
   * @param requestId
   * @param request
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<IprProductsResponse> getIprProductsList(String requestId, IPRProductListRequest request, int page,
      int size);

}
