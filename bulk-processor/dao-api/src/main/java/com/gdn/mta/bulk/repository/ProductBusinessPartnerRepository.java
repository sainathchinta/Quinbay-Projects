package com.gdn.mta.bulk.repository;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;

public interface ProductBusinessPartnerRepository {

  /**
   * Save Product Business partner with activated false return id
   * 
   * @param productBusinessPartner
   * @return
   * @throws Exception
   */
  String saveWithActivatedFalseReturnId(ProductBusinessPartnerRequest productBusinessPartner)
      throws Exception;

  /**
   * Get product business partner by id
   * 
   * @param id
   * @return
   * @throws Exception 
   */
  ProductBusinessPartnerResponse getProductBusinessPartnerById(String id) throws Exception;

  /**
   * Upsert offline items
   *
   * @param requests
   * @param auditTrailInfo
   */
  List<UpsertOfflineItemFailedResponse> upsertOfflineItems(List<UpsertOfflineItemRequest> requests,
      AuditTrailInfo auditTrailInfo) throws Exception;

  /**
   * Delete offline items
   *
   * @param storeId
   * @param requests
   * @param auditTrailInfo
   */
  List<DeleteOfflineItemDetailResponse> deleteOfflineItems(String storeId,
      List<DeleteOfflineItemRequest> requests, AuditTrailInfo auditTrailInfo) throws ApplicationException;

  /**
   * Bulk archive productSkus
   *
   * @param userName
   * @param requestId
   * @param archive
   * @param request
   * @param productSkuErrorMessageMap
   * @return
   */
  List<String> bulkArchiveProductSkus(String userName, String requestId, boolean archive,
      SimpleListStringRequest request, Map<String, String> productSkuErrorMessageMap) throws ApplicationException;

  /**
   * Api to update the product
   *
   * @param productLevel3Request
   * @param hasOrder
   * @param requestId
   * @param username
   * @return
   */
  GdnRestSingleResponse<ProductLevel3Response> updateAndReturn(ProductLevel3Request productLevel3Request,
      boolean hasOrder, String requestId, String username, String clientHost) throws ApplicationException;

  /**
   * Update product category
   *
   * @param productCode
   * @param categoryCode
   * @return
   */
  String updateProductCategory(String productCode, String categoryCode, String username);

  /**
   * Bulk Download Summary
   *
   * @param request
   * @param businessPartnerCode
   * @param fetchViewConfigByChannel
   *
   * @return
   */
  BulkDownloadProductLevel3Response bulkDownloadSummary(ProductLevel3SummaryRequest request, String businessPartnerCode,
      String fetchViewConfigByChannel)
      throws ApplicationException;
}
