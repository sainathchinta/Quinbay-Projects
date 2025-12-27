package com.gdn.mta.bulk.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;

public interface ProductLevel3Repository {

  /**
   * Get all active products for bulk download which are successfully retrieved
   *
   * @param businessPartnerCode
   * @param pageable
   * @param productRequest
   * @return
   * @throws Exception
   */
  BulkDownloadProductLevel3Response findSummaryByFilterForBulkDownload(String businessPartnerCode, Pageable pageable,
      ProductLevel3SummaryRequest productRequest) throws Exception;

  /**
   * Get all active products for bulk download by categoryCode and brands
   *
   * @param pageNumber
   * @param pageSize
   * @param campaignProductDownloadRequest
   * @return
   * @throws Exception
   */
  List<ProductLevel3SummaryResponse> findSummaryByCategoryAndBrand(int pageNumber, int pageSize,
      CampaignProductDownloadRequest campaignProductDownloadRequest) throws Exception;

  /**
   * API to do bulk actions for product suspension
   *
   * @param suspensionProductRequests
   * @param requestId
   * @param username
   * @return
   */
  List<SuspensionProductResponse> doBulkSuspensionProductsActions(List<SuspensionProductRequest> suspensionProductRequests, String requestId, String username)
      throws Exception;
}
